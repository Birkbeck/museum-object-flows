from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, List, Tuple

from mm_db_cloud.config.sheet_config import Instructions, Trash
from mm_db_cloud.models.permanently_delete_museums import (
    PermanentlyDeleteMuseumsRequest,
    PermanentlyDeleteMuseumsResponse,
    RowError,
)


def _is_true_cell(v: Any) -> bool:
    return v is True or (isinstance(v, str) and v.strip().lower() == "true")


class PermanentlyDeleteMuseumsService:
    def __init__(self, sheets_service) -> None:
        self.sheets = sheets_service

    def run(
        self,
        req: PermanentlyDeleteMuseumsRequest,
        spreadsheet_id: str,
    ) -> PermanentlyDeleteMuseumsResponse:
        # Trash data begins row 2 (HEADER_ROW=0)
        rows = self.sheets.get_values(spreadsheet_id, f"{Trash.SHEET_NAME}!A2:ZZ")

        if not rows:
            return PermanentlyDeleteMuseumsResponse(
                ok=True,
                deletedCount=0,
                errorsByRow=[],
                skippedNotMarked=0,
                message="No items to permanently delete.",
            )

        ready_rows: List[Tuple[int, List[Any]]] = []
        skipped_not_marked = 0

        for i, row in enumerate(rows):
            sheet_row_number = Trash.HEADER_ROW + 2 + i  # 1-indexed row
            if not row:
                skipped_not_marked += 1
                continue
            if not _is_true_cell(row[Trash.PERMANENTLY_DELETE]):
                skipped_not_marked += 1
                continue
            ready_rows.append((sheet_row_number, row))

        if not ready_rows:
            return PermanentlyDeleteMuseumsResponse(
                ok=True,
                deletedCount=0,
                errorsByRow=[],
                skippedNotMarked=skipped_not_marked,
                message="No rows marked for permanent deletion.",
            )

        errors_by_row: List[RowError] = []
        deleted_count = 0

        trash_sheet_id = self.sheets.get_sheet_id_by_name(
            spreadsheet_id, Trash.SHEET_NAME
        )

        # delete bottom-up
        ready_rows.sort(key=lambda t: t[0], reverse=True)
        for sheet_row_number, _ in ready_rows:
            try:
                start = sheet_row_number - 1
                end = sheet_row_number
                self.sheets.delete_rows(spreadsheet_id, trash_sheet_id, start, end)
                deleted_count += 1
            except Exception:
                errors_by_row.append(
                    RowError(
                        row=sheet_row_number,
                        errors=["Failed to permanently delete row."],
                    )
                )

        # log change date only if no errors
        if deleted_count > 0 and not errors_by_row:
            now = datetime.now(timezone.utc).isoformat(timespec="seconds")
            self.sheets.batch_update_values(
                spreadsheet_id,
                updates=[
                    (f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}", [[now]])
                ],
            )

        message = (
            f"Permanently deleted {deleted_count} museum."
            if deleted_count == 1
            else f"Permanently deleted {deleted_count} museums."
        )

        return PermanentlyDeleteMuseumsResponse(
            ok=True,
            deletedCount=deleted_count,
            errorsByRow=errors_by_row,
            skippedNotMarked=skipped_not_marked,
            message=message,
        )
