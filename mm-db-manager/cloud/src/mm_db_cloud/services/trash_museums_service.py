from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Tuple

from mm_db_cloud.config.sheet_config import Database, Delete, Instructions, Trash
from mm_db_cloud.models.trash_museums import (
    TrashMuseumsRequest,
    TrashMuseumsResponse,
    RowError,
)
from mm_db_cloud.utils.normalizers import as_trimmed_string, parse_museum_id
from mm_db_cloud.utils.row_mapper import map_db_row_to_db_row


def _is_ready_cell(v: Any) -> bool:
    return v is True or (isinstance(v, str) and v.strip().lower() == "true")


class TrashMuseumsService:
    def __init__(self, sheets_service) -> None:
        self.sheets = sheets_service

    def run(
        self, req: TrashMuseumsRequest, spreadsheet_id: str
    ) -> TrashMuseumsResponse:
        # Read Delete sheet (data begins row 2)
        rows = self.sheets.get_values(spreadsheet_id, f"{Delete.SHEET_NAME}!A2:ZZ")

        if not rows:
            return TrashMuseumsResponse(
                ok=True,
                trashedCount=0,
                errorsByRow=[],
                skippedNotReady=0,
                message="No deletions to commit.",
            )

        ready_rows: List[Tuple[int, List[Any]]] = []
        skipped_not_ready = 0

        for i, row in enumerate(rows):
            sheet_row_number = Delete.HEADER_ROW + 2 + i  # 1-indexed
            if not row:
                skipped_not_ready += 1
                continue
            if not _is_ready_cell(row[Delete.READY_TO_DELETE]):
                skipped_not_ready += 1
                continue
            ready_rows.append((sheet_row_number, row))

        if not ready_rows:
            return TrashMuseumsResponse(
                ok=True,
                trashedCount=0,
                errorsByRow=[],
                skippedNotReady=skipped_not_ready,
                message="No rows marked ready to delete.",
            )

        errors_by_row: List[RowError] = []
        actions: List[Tuple[int, str]] = []  # (delete_sheet_row_number, museum_id)

        # Parse museum IDs (no other validation here, matching TS)
        for sheet_row_number, row in ready_rows:
            try:
                museum_cell = row[Delete.MUSEUM]
                museum_id = parse_museum_id(museum_cell)
            except IndexError:
                museum_id = None
            if not museum_id:
                errors_by_row.append(
                    RowError(
                        row=sheet_row_number,
                        errors=[
                            f'Museum "{as_trimmed_string(museum_cell)}" is not valid. Expected "id - name".'
                        ],
                    )
                )
                continue
            actions.append((sheet_row_number, museum_id))

        if not actions:
            return TrashMuseumsResponse(
                ok=True,
                trashedCount=0,
                errorsByRow=errors_by_row,
                skippedNotReady=skipped_not_ready,
                message="No valid rows marked ready to delete.",
            )

        # Build DB id -> sheet row number map (like buildDbIdRowMap)
        id_to_db_row = self._build_db_id_row_map(spreadsheet_id)

        # Resolve actions to DB row numbers; unresolved become errors
        resolved: List[
            Tuple[int, str, int]
        ] = []  # (delete_sheet_row_number, museum_id, db_row_number)
        for delete_sheet_row_number, museum_id in actions:
            db_row_number = id_to_db_row.get(museum_id)
            if not db_row_number:
                errors_by_row.append(
                    RowError(
                        row=delete_sheet_row_number,
                        errors=[
                            f'Museum ID "{museum_id}" not found in {Database.SHEET_NAME}.'
                        ],
                    )
                )
                continue
            resolved.append((delete_sheet_row_number, museum_id, db_row_number))

        if not resolved:
            return TrashMuseumsResponse(
                ok=True,
                trashedCount=0,
                errorsByRow=errors_by_row,
                skippedNotReady=skipped_not_ready,
                message="No valid rows marked ready to delete.",
            )

        trashed_count = 0

        # Delete DB rows in descending DB row order
        resolved.sort(key=lambda t: t[2], reverse=True)

        delete_sheet_id = self.sheets.get_sheet_id_by_name(
            spreadsheet_id, Delete.SHEET_NAME
        )
        db_sheet_id = self.sheets.get_sheet_id_by_name(
            spreadsheet_id, Database.SHEET_NAME
        )
        trash_sheet_id = self.sheets.get_sheet_id_by_name(
            spreadsheet_id, Trash.SHEET_NAME
        )

        delete_rows_to_remove: List[int] = []

        for delete_sheet_row_number, museum_id, db_row_number in resolved:
            # Read DB row A..AD for that row
            db_row_values = self.sheets.get_values(
                spreadsheet_id,
                f"{Database.SHEET_NAME}!A{db_row_number}:AD{db_row_number}",
            )[0]

            # Find first blank row in Trash in ID column (1-indexed col = Trash.ID + 1)
            trash_row_number = self.sheets.find_first_blank_row(
                spreadsheet_id=spreadsheet_id,
                sheet_name=Trash.SHEET_NAME,
                col_1_indexed=Trash.ID + 1,
                header_row_1_indexed=Trash.HEADER_ROW + 1,
            )

            # Write to Trash at that row (A..AF = 32 cols => A..AF)
            # We assume Trash has 32 cols (0..31), last col is NOTES
            trash_range = f"{Trash.SHEET_NAME}!A{trash_row_number}:AF{trash_row_number}"
            trash_row = map_db_row_to_db_row(
                db_row_values,
                source_sheet_cls=Database,
                dest_sheet_cls=Trash,
            )
            trash_row[Trash.PERMANENTLY_DELETE] = False
            trash_row[Trash.RESTORE] = False
            self.sheets.batch_update_values(
                spreadsheet_id,
                updates=[(trash_range, [trash_row])],
            )

            # Delete row from DB (0-based indices)
            start = db_row_number - 1
            end = db_row_number
            self.sheets.delete_rows(spreadsheet_id, db_sheet_id, start, end)

            trashed_count += 1
            delete_rows_to_remove.append(delete_sheet_row_number)

        # Delete processed rows from Delete sheet bottom-up
        for delete_sheet_row_number in sorted(delete_rows_to_remove, reverse=True):
            start = delete_sheet_row_number - 1
            end = delete_sheet_row_number
            self.sheets.delete_rows(spreadsheet_id, delete_sheet_id, start, end)

        # TS: only log change date if no errors
        if trashed_count > 0 and not errors_by_row:
            now = datetime.now(timezone.utc).isoformat(timespec="seconds")
            self.sheets.batch_update_values(
                spreadsheet_id,
                updates=[
                    (f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}", [[now]])
                ],
            )

        message = (
            f"Moved {trashed_count} museum to Trash."
            if trashed_count == 1
            else f"Moved {trashed_count} museums to Trash."
        )

        return TrashMuseumsResponse(
            ok=True,
            trashedCount=trashed_count,
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
