from __future__ import annotations

from typing import Any, Dict, List, Tuple

import pytest

from mm_db_cloud.config.sheet_config import Database, Delete, Instructions, Trash
from mm_db_cloud.models.trash_museums import TrashMuseumsRequest
from mm_db_cloud.services.trash_museums_service import TrashMuseumsService


class FakeSheetsService:
    def __init__(self) -> None:
        self.values: Dict[Tuple[str, str], List[List[Any]]] = {}
        self.sheet_ids: Dict[Tuple[str, str], int] = {}
        self.append_calls: List[Tuple[str, str, List[Any]]] = []
        self.delete_calls: List[Tuple[str, int, int, int]] = []
        self.batch_updates: List[Tuple[str, List[Tuple[str, List[List[Any]]]]]] = []
        self.find_blank_calls: List[Tuple[str, str, int, int, int]] = []

    def get_values(self, spreadsheet_id: str, range_a1: str) -> List[List[Any]]:
        return self.values.get((spreadsheet_id, range_a1), [])

    def get_sheet_id_by_name(self, spreadsheet_id: str, sheet_name: str) -> int:
        return self.sheet_ids[(spreadsheet_id, sheet_name)]

    def append_row(
        self, spreadsheet_id: str, sheet_name: str, row_values: List[Any]
    ) -> Dict[str, Any]:
        self.append_calls.append((spreadsheet_id, sheet_name, row_values))
        return {"ok": True}

    def delete_rows(
        self, spreadsheet_id: str, sheet_id: int, start_index: int, end_index: int
    ) -> Dict[str, Any]:
        self.delete_calls.append((spreadsheet_id, sheet_id, start_index, end_index))
        return {"ok": True}

    def batch_update_values(
        self, spreadsheet_id: str, updates: List[Tuple[str, List[List[Any]]]]
    ) -> Dict[str, Any]:
        self.batch_updates.append((spreadsheet_id, updates))
        return {"ok": True}

    # New SheetsService API
    def col_to_a1(self, col_1_indexed: int) -> str:
        if col_1_indexed < 1:
            raise ValueError("col_1_indexed must be >= 1")
        n = col_1_indexed
        letters = ""
        while n > 0:
            n, rem = divmod(n - 1, 26)
            letters = chr(ord("A") + rem) + letters
        return letters

    def find_first_blank_row(
        self,
        spreadsheet_id: str,
        sheet_name: str,
        col_1_indexed: int,
        header_row_1_indexed: int,
        scan_limit: int = 2000,
    ) -> int:
        # record call for assertions
        self.find_blank_calls.append(
            (
                spreadsheet_id,
                sheet_name,
                col_1_indexed,
                header_row_1_indexed,
                scan_limit,
            )
        )
        start_row = header_row_1_indexed + 1
        col_letter = self.col_to_a1(col_1_indexed)
        range_a1 = f"{sheet_name}!{col_letter}{start_row}:{col_letter}{start_row + scan_limit - 1}"
        vals = self.get_values(spreadsheet_id, range_a1)

        for i, row in enumerate(vals):
            v = row[0] if row else ""
            if str(v or "").strip() == "":
                return start_row + i

        return start_row + len(vals)


def test_trash_museums_happy_path_moves_db_row_to_trash_and_deletes(monkeypatch):
    ssid = "S1"
    sheets = FakeSheetsService()

    # Delete sheet: one ready row referencing mm.new.2
    sheets.values[(ssid, f"{Delete.SHEET_NAME}!A2:ZZ")] = [
        [True, "mm.new.2 - Museum Two"],
    ]

    # DB id map: mm.new.1 at row2, mm.new.2 at row3
    sheets.values[(ssid, f"{Database.SHEET_NAME}!A2:A")] = [["mm.new.1"], ["mm.new.2"]]

    # DB row read for row3 (A3:AD3)
    sheets.values[(ssid, f"{Database.SHEET_NAME}!A3:AD3")] = [
        ["mm.new.2"] + ["X"] * (Database.NOTES)
    ]

    # Trash first-blank scan range (ID column => C, starting from row 2)
    sheets.values[(ssid, f"{Trash.SHEET_NAME}!C2:C2001")] = [[""]]

    sheets.sheet_ids[(ssid, Delete.SHEET_NAME)] = 10
    sheets.sheet_ids[(ssid, Database.SHEET_NAME)] = 20
    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 30

    # Patch mapper so we can assert exactly what gets written to Trash
    def _fake_map_db_row_to_db_row(source_row, *, source_sheet_cls, dest_sheet_cls):
        out = [""] * dest_sheet_cls.TOTAL_COLS
        out[dest_sheet_cls.ID] = source_row[source_sheet_cls.ID]
        out[dest_sheet_cls.MUSEUM_NAME] = "Museum Two"
        return out

    monkeypatch.setattr(
        "mm_db_cloud.services.trash_museums_service.map_db_row_to_db_row",
        _fake_map_db_row_to_db_row,
    )

    svc = TrashMuseumsService(sheets)
    resp = svc.run(TrashMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.trashedCount == 1
    assert resp.skippedNotReady == 0
    assert resp.errorsByRow == []

    # find_first_blank_row should be called for Trash ID column
    assert sheets.find_blank_calls == [
        (ssid, Trash.SHEET_NAME, Trash.ID + 1, Trash.HEADER_ROW + 1, 2000)
    ]

    # ---- Assert Trash write happened via batch_update_values ----
    # Expect write to Trash row 2 (because first blank returned 2) over A..AF
    trash_writes = [
        (rng, vals)
        for _sid, updates in sheets.batch_updates
        for (rng, vals) in updates
        if rng.startswith(f"{Trash.SHEET_NAME}!")
    ]
    assert len(trash_writes) == 1

    written_range, written_values = trash_writes[0]
    assert written_range == f"{Trash.SHEET_NAME}!A2:AF2"
    assert isinstance(written_values, list) and len(written_values) == 1
    written_row = written_values[0]
    assert written_row[Trash.ID] == "mm.new.2"
    assert written_row[Trash.MUSEUM_NAME] == "Museum Two"
    assert written_row[Trash.PERMANENTLY_DELETE] is False
    assert written_row[Trash.RESTORE] is False

    # ---- Assert deletions ----
    # DB row 3 => 0-based [2,3) on sheetId=20
    # Delete sheet row 2 => 0-based [1,2) on sheetId=10
    assert sheets.delete_calls == [
        (ssid, 20, 2, 3),
        (ssid, 10, 1, 2),
    ]

    # ---- Assert Instructions date logged (since no errors) ----
    instr_writes = [
        (rng, vals)
        for _sid, updates in sheets.batch_updates
        for (rng, vals) in updates
        if rng == f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}"
    ]
    assert len(instr_writes) == 1


def test_trash_museums_bad_museum_cell_errors_and_does_nothing():
    ssid = "S1"
    sheets = FakeSheetsService()

    sheets.values[(ssid, f"{Delete.SHEET_NAME}!A2:ZZ")] = [
        [True, "not a valid id-name cell"],
    ]

    sheets.sheet_ids[(ssid, Delete.SHEET_NAME)] = 10
    sheets.sheet_ids[(ssid, Database.SHEET_NAME)] = 20
    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 30

    svc = TrashMuseumsService(sheets)
    resp = svc.run(TrashMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.trashedCount == 0
    assert len(resp.errorsByRow) == 1

    assert sheets.append_calls == []
    assert sheets.delete_calls == []
    assert sheets.batch_updates == []


def test_trash_museums_id_not_found_in_db_is_error_and_does_not_write():
    ssid = "S1"
    sheets = FakeSheetsService()

    sheets.values[(ssid, f"{Delete.SHEET_NAME}!A2:ZZ")] = [
        [True, "mm.new.999 - Missing Museum"],
    ]

    # DB id map does not include mm.new.999
    sheets.values[(ssid, f"{Database.SHEET_NAME}!A2:A")] = [["mm.new.1"]]

    sheets.sheet_ids[(ssid, Delete.SHEET_NAME)] = 10
    sheets.sheet_ids[(ssid, Database.SHEET_NAME)] = 20
    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 30

    svc = TrashMuseumsService(sheets)
    resp = svc.run(TrashMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.trashedCount == 0
    assert len(resp.errorsByRow) == 1
    assert "not found" in resp.errorsByRow[0].errors[0].lower()

    assert sheets.append_calls == []
    assert sheets.delete_calls == []
    assert sheets.batch_updates == []
