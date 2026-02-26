from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Tuple

from mm_db_cloud.config.sheet_config import Database, Instructions, Trash
from mm_db_cloud.models.restore_museums import (
    RestoreMuseumsRequest,
    RestoreMuseumsResponse,
    RowError,
)
from mm_db_cloud.utils.normalizers import as_trimmed_string
from mm_db_cloud.utils.row_mapper import map_db_row_to_db_row


def _is_ready_cell(v: Any) -> bool:
    return v is True or (isinstance(v, str) and v.strip().lower() == "true")


class RestoreMuseumsService:
    def __init__(self, sheets_service) -> None:
        self.sheets = sheets_service

    def run(
        self, req: RestoreMuseumsRequest, spreadsheet_id: str
    ) -> RestoreMuseumsResponse:
        rows = self.sheets.get_values(spreadsheet_id, f"{Trash.SHEET_NAME}!A2:ZZ")

        if not rows:
            return RestoreMuseumsResponse(
                ok=True,
                restoredCount=0,
                errorsByRow=[],
                skippedNotReady=0,
                message="No restores to commit.",
            )

        ready_rows: List[Tuple[int, List[Any]]] = []
        skipped_not_ready = 0

        for i, row in enumerate(rows):
            sheet_row_number = Trash.HEADER_ROW + 2 + i  # 1-indexed
            if not row:
                skipped_not_marked += 1
                continue
            if not _is_ready_cell(row[Trash.RESTORE]):
                skipped_not_ready += 1
                continue
            ready_rows.append((sheet_row_number, row))

        if not ready_rows:
            return RestoreMuseumsResponse(
                ok=True,
                restoredCount=0,
                errorsByRow=[],
                skippedNotReady=skipped_not_ready,
                message="No rows marked ready to restore.",
            )

        errors_by_row: List[RowError] = []
        actions: List[
            Tuple[int, str, List[Any]]
        ] = []  # (trash_row_number, museum_id, row)

        for sheet_row_number, row in ready_rows:
            museum_id = as_trimmed_string(row[Trash.ID])
            if not museum_id:
                errors_by_row.append(
                    RowError(
                        row=sheet_row_number,
                        errors=["Trash row is missing a Museum ID."],
                    )
                )
                continue
            actions.append((sheet_row_number, museum_id, row))

        if not actions:
            return RestoreMuseumsResponse(
                ok=True,
                restoredCount=0,
                errorsByRow=errors_by_row,
                skippedNotReady=skipped_not_ready,
                message="No valid rows marked ready to restore.",
            )

        id_to_db_row = self._build_db_id_row_map(spreadsheet_id)

        restored_count = 0
        trash_sheet_id = self.sheets.get_sheet_id_by_name(
            spreadsheet_id, Trash.SHEET_NAME
        )

        # Deletes bottom-up on Trash sheet
        for sheet_row_number, museum_id, row in sorted(
            actions, key=lambda x: x[0], reverse=True
        ):
            if museum_id in id_to_db_row:
                errors_by_row.append(
                    RowError(
                        row=sheet_row_number,
                        errors=[
                            f'Museum ID "{museum_id}" already exists in {Database.SHEET_NAME}.'
                        ],
                    )
                )
                continue

            db_row = map_db_row_to_db_row(
                row,
                source_sheet_cls=Trash,
                dest_sheet_cls=Database,
            )
            self.sheets.append_row(spreadsheet_id, Database.SHEET_NAME, db_row)

            # Update local map so repeated restores in same run
            # (i.e. second row restoring same id will be caught)
            id_to_db_row[museum_id] = -1

            # Delete restored trash row
            start = sheet_row_number - 1
            end = sheet_row_number
            self.sheets.delete_rows(spreadsheet_id, trash_sheet_id, start, end)

            restored_count += 1

        # TS: only log change date if no errors
        if restored_count > 0 and not errors_by_row:
            now = datetime.now(timezone.utc).isoformat(timespec="seconds")
            self.sheets.batch_update_values(
                spreadsheet_id,
                updates=[
                    (f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}", [[now]])
                ],
            )

        message = (
            f"Restored {restored_count} museum to Database."
            if restored_count == 1
            else f"Restored {restored_count} museums to Database."
        )

        return RestoreMuseumsResponse(
            ok=True,
            restoredCount=restored_count,
            errorsByRow=errors_by_row,
            skippedNotReady=skipped_not_ready,
            message=message,
        )

    def _build_db_id_row_map(self, spreadsheet_id: str) -> Dict[str, int]:
        id_values = self.sheets.get_values(
            spreadsheet_id, f"{Database.SHEET_NAME}!A2:A"
        )
        mapping: Dict[str, int] = {}
        for i, row in enumerate(id_values):
            museum_id = as_trimmed_string(row[0])
            if not museum_id:
                continue
            mapping[museum_id] = Database.HEADER_ROW + 2 + i
        return mapping
