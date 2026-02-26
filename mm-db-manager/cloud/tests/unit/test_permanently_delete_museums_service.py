from __future__ import annotations

from typing import Any, Dict, List, Tuple

from mm_db_cloud.config.sheet_config import Instructions, Trash
from mm_db_cloud.models.permanently_delete_museums import (
    PermanentlyDeleteMuseumsRequest,
)
from mm_db_cloud.services.permanently_delete_museums_service import (
    PermanentlyDeleteMuseumsService,
)


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


def test_permanently_delete_no_items_to_delete():
    ssid = "S1"
    sheets = FakeSheetsService()
    sheets.values[(ssid, f"{Trash.SHEET_NAME}!A2:ZZ")] = []

    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 30

    svc = PermanentlyDeleteMuseumsService(sheets)
    resp = svc.run(PermanentlyDeleteMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.deletedCount == 0
    assert resp.errorsByRow == []
    assert resp.message == "No items to permanently delete."
    assert sheets.delete_calls == []
    # no logging
    instr_writes = [
        (rng, vals)
        for _sid, updates in sheets.batch_updates
        for (rng, vals) in updates
        if rng == f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}"
    ]
    assert instr_writes == []


def test_permanently_delete_none_marked():
    ssid = "S1"
    sheets = FakeSheetsService()

    sheets.values[(ssid, f"{Trash.SHEET_NAME}!A2:ZZ")] = [
        [False, False, "mm.new.1"],
        ["FALSE", "", "mm.new.2"],
        ["", None, "mm.new.3"],
    ]
    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 30

    svc = PermanentlyDeleteMuseumsService(sheets)
    resp = svc.run(PermanentlyDeleteMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.deletedCount == 0
    assert resp.errorsByRow == []
    assert resp.message == "No rows marked for permanent deletion."
    assert resp.skippedNotMarked == 3
    assert sheets.delete_calls == []
    # no logging
    assert sheets.batch_updates == []


def test_permanently_delete_happy_path_deletes_bottom_up_and_logs_date():
    ssid = "S1"
    sheets = FakeSheetsService()

    # Rows correspond to sheet rows 2,3,4 (HEADER_ROW=0)
    sheets.values[(ssid, f"{Trash.SHEET_NAME}!A2:ZZ")] = [
        [True, "x"],  # row 2 -> delete
        [False, "y"],  # row 3 -> skip
        [True, "z"],  # row 4 -> delete
    ]
    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 30

    svc = PermanentlyDeleteMuseumsService(sheets)
    resp = svc.run(PermanentlyDeleteMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.deletedCount == 2
    assert resp.errorsByRow == []
    assert resp.skippedNotMarked == 1
    assert resp.message == "Permanently deleted 2 museums."

    # bottom-up: delete row 4 first => start=3,end=4 ; then row 2 => start=1,end=2
    assert sheets.delete_calls == [
        (ssid, 30, 3, 4),
        (ssid, 30, 1, 2),
    ]

    instr_writes = [
        (rng, vals)
        for _sid, updates in sheets.batch_updates
        for (rng, vals) in updates
        if rng == f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}"
    ]
    assert len(instr_writes) == 1
    # value is an ISO datetime string; don’t over-assert exact timestamp
    assert isinstance(instr_writes[0][1], list)


def test_permanently_delete_records_errors_and_does_not_log_date(monkeypatch):
    ssid = "S1"
    sheets = FakeSheetsService()

    sheets.values[(ssid, f"{Trash.SHEET_NAME}!A2:ZZ")] = [
        [True, "x"],  # row 2 delete will fail
        [True, "y"],  # row 3 delete ok
    ]
    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 30

    # Make delete_rows raise only for the first call (row 3 is deleted first, bottom-up).
    # ready rows are 3 then 2; we want failure on row 2 -> second call.
    original_delete_rows = sheets.delete_rows

    call_count = {"n": 0}

    def _delete_rows(ssid_, sheet_id, start, end):
        call_count["n"] += 1
        if call_count["n"] == 2:  # second delete attempt
            raise RuntimeError("boom")
        return original_delete_rows(ssid_, sheet_id, start, end)

    monkeypatch.setattr(sheets, "delete_rows", _delete_rows)

    svc = PermanentlyDeleteMuseumsService(sheets)
    resp = svc.run(PermanentlyDeleteMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.deletedCount == 1
    assert len(resp.errorsByRow) == 1
    assert resp.errorsByRow[0].row == 2
    assert resp.errorsByRow[0].errors == ["Failed to permanently delete row."]

    # no Instructions logging when errors exist
    instr_writes = [
        (rng, vals)
        for _sid, updates in sheets.batch_updates
        for (rng, vals) in updates
        if rng == f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}"
    ]
    assert instr_writes == []
