from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Tuple

from mm_db_cloud.models.edit_museums import (
    EditMuseumsRequest,
    EditMuseumsResponse,
    RowError,
)
from mm_db_cloud.utils.validators import validate_form_row
from mm_db_cloud.utils.normalizers import as_trimmed_string, parse_museum_id
from mm_db_cloud.utils.row_mapper import map_form_row_to_db_row
from mm_db_cloud.config.sheet_config import Database, Edit, Instructions


def _is_ready_cell(v: Any) -> bool:
    return v is True or (isinstance(v, str) and v.strip().lower() == "true")


class EditMuseumsService:
    def __init__(self, sheets_service) -> None:
        self.sheets = sheets_service

    def run(self, req: EditMuseumsRequest, spreadsheet_id: str) -> EditMuseumsResponse:
        # Read Edit sheet (data starts at row 2)
        rows = self.sheets.get_values(
            spreadsheet_id,
            f"{Edit.SHEET_NAME}!A2:ZZ",
        )

        if not rows:
            return EditMuseumsResponse(
                ok=True,
                editedCount=0,
                errorsByRow=[],
                skippedNotReady=0,
                message="No edits to commit.",
            )

        ready_rows: List[Tuple[int, List[Any]]] = []
        skipped_not_ready = 0

        for i, row in enumerate(rows):
            sheet_row_number = Edit.HEADER_ROW + 2 + i  # 1-indexed
            if not row:
                skipped_not_ready += 1
                continue
            if not _is_ready_cell(row[Edit.READY_TO_COMMIT]):
                skipped_not_ready += 1
                continue
            ready_rows.append((sheet_row_number, row))

        if not ready_rows:
            return EditMuseumsResponse(
                ok=True,
                editedCount=0,
                errorsByRow=[],
                skippedNotReady=skipped_not_ready,
                message="No rows marked ready to commit.",
            )

        errors_by_row: List[RowError] = []
        actions: List[Tuple[int, str, List[Any]]] = []

        # Validate rows + parse museum IDs
        for sheet_row_number, row in ready_rows:
            errs: List[str] = []
            try:
                museum_cell = row[Edit.MUSEUM]
                museum_id = parse_museum_id(museum_cell)
            except IndexError:
                museum_id = None
            if not museum_id:
                errs.append(
                    f'Museum "{as_trimmed_string(museum_cell)}" is not valid. '
                    'Expected "id - name".'
                )

            errs.extend(validate_form_row(row, Edit))

            if errs:
                errors_by_row.append(RowError(row=sheet_row_number, errors=errs))
                continue

            actions.append((sheet_row_number, museum_id, row))

        if not actions:
            return EditMuseumsResponse(
                ok=True,
                editedCount=0,
                errorsByRow=errors_by_row,
                skippedNotReady=skipped_not_ready,
                message="No valid rows marked ready to commit.",
            )

        # Build DB ID → row number map
        id_to_row = self._build_db_id_row_map(spreadsheet_id)

        edited_count = 0
        edit_sheet_id = self.sheets.get_sheet_id_by_name(
            spreadsheet_id, Edit.SHEET_NAME
        )

        # Process bottom-up
        for sheet_row_number, museum_id, row in sorted(
            actions, key=lambda x: x[0], reverse=True
        ):
            db_row_number = id_to_row.get(museum_id)
            if not db_row_number:
                errors_by_row.append(
                    RowError(
                        row=sheet_row_number,
                        errors=[
                            f'Museum ID "{museum_id}" not found in {Database.SHEET_NAME}.'
                        ],
                    )
                )
                continue

            db_row = map_form_row_to_db_row(
                row,
                museum_id=museum_id,
                form_sheet_cls=Edit,
            )

            a1 = f"{Database.SHEET_NAME}!A{db_row_number}:AD{db_row_number}"
            self.sheets.batch_update_values(
                spreadsheet_id,
                updates=[(a1, [db_row])],
            )

            # Delete edit row (0-based indices)
            start = sheet_row_number - 1
            end = sheet_row_number
            self.sheets.delete_rows(spreadsheet_id, edit_sheet_id, start, end)

            edited_count += 1

        # Only log change date if there were no errors
        if edited_count > 0 and not errors_by_row:
            now = datetime.now(timezone.utc).isoformat(timespec="seconds")
            self.sheets.batch_update_values(
                spreadsheet_id,
                updates=[
                    (f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}", [[now]])
                ],
            )

        message = (
            f"Edited {edited_count} museum in Database."
            if edited_count == 1
            else f"Edited {edited_count} museums in Database."
        )

        return EditMuseumsResponse(
            ok=True,
            editedCount=edited_count,
            errorsByRow=errors_by_row,
            skippedNotReady=skipped_not_ready,
            message=message,
        )

    def _build_db_id_row_map(self, spreadsheet_id: str) -> Dict[str, int]:
        id_values = self.sheets.get_values(
            spreadsheet_id,
            f"{Database.SHEET_NAME}!A2:A",
        )

        mapping: Dict[str, int] = {}
        for i, row in enumerate(id_values):
            museum_id = as_trimmed_string(row[0])
            if not museum_id:
                continue
            sheet_row_number = Database.HEADER_ROW + 2 + i
            mapping[museum_id] = sheet_row_number

        return mapping
