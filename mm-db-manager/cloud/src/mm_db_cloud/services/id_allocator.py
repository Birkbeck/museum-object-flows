from __future__ import annotations

import re
from typing import Any, List, Optional

from googleapiclient.errors import HttpError

from mm_db_cloud.config.sheet_config import Database, Trash, NewIds

_MM_NEW_RE = re.compile(r"\bmm\.new\.(\d+)\b", re.IGNORECASE)


class IdAllocator:
    def __init__(self, sheets_service) -> None:
        self.sheets = sheets_service

    def allocate_next_ids(self, spreadsheet_id: str, count: int) -> List[str]:
        """
        Allocate `count` IDs, updating New IDs!A1 as a counter.

        Returns: ["mm.new.<n+1>", ..., "mm.new.<n+count>"]
        """
        if count <= 0:
            return []

        # Ensure New IDs sheet exists + seeded
        self._ensure_id_sheet_seeded(spreadsheet_id)

        # Read counter cell
        counter_range = f"{NewIds.SHEET_NAME}!{NewIds.LAST_ID}"
        raw = self.sheets.get_single_value(spreadsheet_id, counter_range)
        last_issued = self._parse_counter_cell(raw)

        next_val = last_issued + 1
        new_last = last_issued + count

        # Write updated counter back
        self.sheets.batch_update_values(
            spreadsheet_id,
            updates=[(counter_range, [[new_last]])],
        )

        return [f"mm.new.{n}" for n in range(next_val, new_last + 1)]

    def _ensure_id_sheet_seeded(self, spreadsheet_id: str) -> None:
        """
        If New IDs sheet doesn't exist, create it, seed A1 with max mm.new.<n>
        found in DB + Trash, and hide it.
        """
        try:
            sheet_id = self.sheets.get_sheet_id_by_name(
                spreadsheet_id, NewIds.SHEET_NAME
            )
            # Sheet exists; nothing else to do.
            return
        except Exception:
            sheet_id = None

        # Create sheet
        self.sheets.create_sheet(spreadsheet_id, NewIds.SHEET_NAME)
        sheet_id = self.sheets.get_sheet_id_by_name(spreadsheet_id, NewIds.SHEET_NAME)

        # Seed with max existing found in DB + Trash
        max_existing = self._find_max_mm_new_suffix_in_db_and_trash(spreadsheet_id)
        counter_range = f"{NewIds.SHEET_NAME}!{NewIds.LAST_ID}"
        self.sheets.batch_update_values(
            spreadsheet_id,
            updates=[(counter_range, [[max_existing]])],
        )

        # Hide it (best-effort)
        try:
            self.sheets.hide_sheet(spreadsheet_id, sheet_id)
        except Exception:
            pass

    def _find_max_mm_new_suffix_in_db_and_trash(self, spreadsheet_id: str) -> int:
        # DB IDs are column A
        db_vals = self.sheets.get_values(spreadsheet_id, f"{Database.SHEET_NAME}!A:Z")
        trash_vals = self.sheets.get_values(spreadsheet_id, f"{Trash.SHEET_NAME}!A:AF")
        return max(
            self._max_suffix_in_grid(db_vals), self._max_suffix_in_grid(trash_vals)
        )

    def _max_suffix_in_grid(self, grid: List[List[Any]]) -> int:
        m = 0
        for row in grid:
            for cell in row:
                if cell is None or cell == "":
                    continue
                s = str(cell)
                match = _MM_NEW_RE.search(s)
                if not match:
                    continue
                try:
                    n = int(match.group(1))
                except ValueError:
                    continue
                if n >= 0 and n > m:
                    m = n
        return m

    def _parse_counter_cell(self, value: Any) -> int:
        if isinstance(value, bool):
            raise ValueError(f"Invalid New IDs counter value: {value!r}")

        if isinstance(value, int):
            if value >= 0:
                return value
            raise ValueError(f"Invalid New IDs counter value: {value!r}")

        if isinstance(value, float):
            if value.is_integer() and value >= 0:
                return int(value)
            raise ValueError(f"Invalid New IDs counter value: {value!r}")

        if value is None or value == "":
            return 0

        if isinstance(value, str):
            s = value.strip()
            if s == "":
                return 0
            try:
                n = int(s)
            except ValueError:
                raise ValueError(f"Invalid New IDs counter value: {value!r}")
            if n >= 0:
                return n
            raise ValueError(f"Invalid New IDs counter value: {value!r}")

        raise ValueError(f"Invalid New IDs counter value: {value!r}")
