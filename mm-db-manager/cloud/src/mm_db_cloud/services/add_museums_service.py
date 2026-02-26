from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, List, Tuple

from mm_db_cloud.models.add_museums import (
    AddMuseumsRequest,
    AddMuseumsResponse,
    RowError,
)
from mm_db_cloud.services.id_allocator import IdAllocator
from mm_db_cloud.utils.row_mapper import map_form_row_to_db_row
from mm_db_cloud.utils.validators import validate_form_row
from mm_db_cloud.config.sheet_config import Add, Database, Instructions


def _is_ready_cell(v: Any) -> bool:
    return v is True or (isinstance(v, str) and v.strip().lower() == "true")


class AddMuseumsService:
    def __init__(self, sheets_service) -> None:
        self.sheets = sheets_service
        self.id_allocator = IdAllocator(sheets_service)

    def run(self, req: AddMuseumsRequest, spreadsheet_id: str) -> AddMuseumsResponse:
        rows = self.sheets.get_values(
            spreadsheet_id,
            f"{Add.SHEET_NAME}!A2:Z",
        )

        if not rows:
            return AddMuseumsResponse(
                ok=True,
                addedCount=0,
                errorsByRow=[],
                skippedNotReady=0,
                message="No rows to add.",
            )

        ready_rows: List[Tuple[int, List[Any]]] = []
        skipped_not_ready = 0

        for i, row in enumerate(rows):
            sheet_row_number = Add.HEADER_ROW + 2 + i  # 1-indexed
            if not row:
                skipped_not_ready += 1
                continue
            if not _is_ready_cell(row[Add.READY_TO_COMMIT]):
                skipped_not_ready += 1
                continue
            ready_rows.append((sheet_row_number, row))

        if not ready_rows:
            return AddMuseumsResponse(
                ok=True,
                addedCount=0,
                errorsByRow=[],
                skippedNotReady=skipped_not_ready,
                message="No rows marked ready to commit.",
            )

        errors_by_row: List[RowError] = []
        actions: List[Tuple[int, List[Any]]] = []

        for sheet_row_number, row in ready_rows:
            errs = validate_form_row(row, Add)
            if errs:
                errors_by_row.append(RowError(row=sheet_row_number, errors=errs))
            else:
                actions.append((sheet_row_number, row))

        if not actions:
            return AddMuseumsResponse(
                ok=True,
                addedCount=0,
                errorsByRow=errors_by_row,
                skippedNotReady=skipped_not_ready,
                message="No valid rows marked ready to commit.",
            )

        # Allocate IDs (sequential)
        new_ids = self.id_allocator.allocate_next_ids(
            spreadsheet_id=spreadsheet_id, count=len(actions)
        )

        added_count = 0

        # Append rows
        for museum_id, (_sheet_row_number, add_row) in zip(new_ids, actions):
            db_row = map_form_row_to_db_row(
                add_row, museum_id=museum_id, form_sheet_cls=Add
            )
            self.sheets.append_row(spreadsheet_id, Database.SHEET_NAME, db_row)
            added_count += 1

        # Delete committed rows bottom-up so indices don't shift
        add_sheet_id = self.sheets.get_sheet_id_by_name(spreadsheet_id, Add.SHEET_NAME)
        for sheet_row_number, _ in sorted(actions, key=lambda x: x[0], reverse=True):
            start = sheet_row_number - 1
            end = sheet_row_number
            self.sheets.delete_rows(spreadsheet_id, add_sheet_id, start, end)

        if added_count > 0 and not errors_by_row:
            now = datetime.now(timezone.utc).isoformat(timespec="seconds")
            self.sheets.batch_update_values(
                spreadsheet_id,
                updates=[
                    (f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}", [[now]])
                ],
            )

        message = (
            f"Added {added_count} museum to Database."
            if added_count == 1
            else f"Added {added_count} museums to Database."
        )

        return AddMuseumsResponse(
            ok=True,
            addedCount=added_count,
            errorsByRow=errors_by_row,
            skippedNotReady=skipped_not_ready,
            message=message,
        )
