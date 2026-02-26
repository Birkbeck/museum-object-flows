from __future__ import annotations

from typing import Any, Dict, List, Tuple

import pytest

from mm_db_cloud.config.sheet_config import Database, Instructions, Trash
from mm_db_cloud.models.restore_museums import RestoreMuseumsRequest
from mm_db_cloud.services.restore_museums_service import RestoreMuseumsService


class FakeSheetsService:
    def __init__(self) -> None:
        self.values: Dict[Tuple[str, str], List[List[Any]]] = {}
        self.sheet_ids: Dict[Tuple[str, str], int] = {}
        self.append_calls: List[Tuple[str, str, List[Any]]] = []
        self.delete_calls: List[Tuple[str, int, int, int]] = []
        self.batch_updates: List[Tuple[str, List[Tuple[str, List[List[Any]]]]]] = []

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


def _trash_row_with_id(museum_id: str, restore: bool = True) -> List[Any]:
    row = [""] * (Trash.NOTES + 1)
    row[Trash.RESTORE] = restore
    row[Trash.ID] = museum_id
    return row


def test_restore_happy_path_appends_and_deletes_bottom_up(monkeypatch):
    ssid = "S1"
    sheets = FakeSheetsService()

    # Two ready rows in Trash (these appear as A2 and A3)
    sheets.values[(ssid, f"{Trash.SHEET_NAME}!A2:ZZ")] = [
        _trash_row_with_id("mm.new.1"),
        _trash_row_with_id("mm.new.2"),
    ]

    # DB already has something else, but not the two IDs
    sheets.values[(ssid, f"{Database.SHEET_NAME}!A2:A")] = [["mm.new.999"]]

    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 55

    # Patch db->db mapper to deterministic output
    monkeypatch.setattr(
        "mm_db_cloud.services.restore_museums_service.map_db_row_to_db_row",
        lambda source_row, source_sheet_cls, dest_sheet_cls: [
            "DBROW",
            source_row[source_sheet_cls.ID],
        ],
    )

    svc = RestoreMuseumsService(sheets)
    resp = svc.run(RestoreMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.restoredCount == 2
    assert resp.errorsByRow == []
    assert resp.skippedNotReady == 0

    # Appended twice to DB
    assert len(sheets.append_calls) == 2
    assert all(call[1] == Database.SHEET_NAME for call in sheets.append_calls)

    # Deletes bottom-up on Trash: row3 first, then row2
    # row3 => start=2,end=3; row2 => start=1,end=2 (0-based)
    assert sheets.delete_calls == [
        (ssid, 55, 2, 3),
        (ssid, 55, 1, 2),
    ]

    # Instructions date logged once (because no errors)
    assert any(
        updates and updates[0][0] == f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}"
        for _sid, updates in sheets.batch_updates
    )


def test_restore_skips_not_ready_rows():
    ssid = "S1"
    sheets = FakeSheetsService()

    sheets.values[(ssid, f"{Trash.SHEET_NAME}!A2:ZZ")] = [
        _trash_row_with_id("mm.new.1", restore=False),
        _trash_row_with_id("mm.new.2", restore=False),
    ]
    sheets.values[(ssid, f"{Database.SHEET_NAME}!A2:A")] = []
    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 55

    svc = RestoreMuseumsService(sheets)
    resp = svc.run(RestoreMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.restoredCount == 0
    assert resp.skippedNotReady == 2
    assert resp.errorsByRow == []
    assert sheets.append_calls == []
    assert sheets.delete_calls == []
    assert sheets.batch_updates == []


def test_restore_missing_id_is_error_and_does_not_write():
    ssid = "S1"
    sheets = FakeSheetsService()

    row = _trash_row_with_id("", restore=True)
    sheets.values[(ssid, f"{Trash.SHEET_NAME}!A2:ZZ")] = [row]
    sheets.values[(ssid, f"{Database.SHEET_NAME}!A2:A")] = []
    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 55

    svc = RestoreMuseumsService(sheets)
    resp = svc.run(RestoreMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.restoredCount == 0
    assert len(resp.errorsByRow) == 1
    assert "missing a Museum ID" in resp.errorsByRow[0].errors[0]

    assert sheets.append_calls == []
    assert sheets.delete_calls == []
    assert sheets.batch_updates == []


def test_restore_id_already_exists_errors_and_does_not_delete_or_append(monkeypatch):
    ssid = "S1"
    sheets = FakeSheetsService()

    sheets.values[(ssid, f"{Trash.SHEET_NAME}!A2:ZZ")] = [
        _trash_row_with_id("mm.new.1", restore=True),
    ]
    # DB already contains mm.new.1
    sheets.values[(ssid, f"{Database.SHEET_NAME}!A2:A")] = [["mm.new.1"]]
    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 55

    # Mapper should never be called since we bail out before appending
    monkeypatch.setattr(
        "mm_db_cloud.services.restore_museums_service.map_db_row_to_db_row",
        lambda *args, **kwargs: (_ for _ in ()).throw(
            AssertionError("mapper should not be called")
        ),
    )

    svc = RestoreMuseumsService(sheets)
    resp = svc.run(RestoreMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.restoredCount == 0
    assert len(resp.errorsByRow) == 1
    assert "already exists" in resp.errorsByRow[0].errors[0]

    assert sheets.append_calls == []
    assert sheets.delete_calls == []
    # No instructions log when errors exist
    assert sheets.batch_updates == []


def test_restore_mixed_success_and_error_does_not_log_change_date(monkeypatch):
    ssid = "S1"
    sheets = FakeSheetsService()

    # Two ready rows; one duplicates an existing id
    sheets.values[(ssid, f"{Trash.SHEET_NAME}!A2:ZZ")] = [
        _trash_row_with_id("mm.new.1", restore=True),
        _trash_row_with_id("mm.new.2", restore=True),
    ]
    sheets.values[(ssid, f"{Database.SHEET_NAME}!A2:A")] = [
        ["mm.new.2"]
    ]  # mm.new.2 already exists
    sheets.sheet_ids[(ssid, Trash.SHEET_NAME)] = 55

    monkeypatch.setattr(
        "mm_db_cloud.services.restore_museums_service.map_db_row_to_db_row",
        lambda source_row, source_sheet_cls, dest_sheet_cls: [
            "DBROW",
            source_row[source_sheet_cls.ID],
        ],
    )

    svc = RestoreMuseumsService(sheets)
    resp = svc.run(RestoreMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.restoredCount == 1
    assert len(resp.errorsByRow) == 1
    assert "already exists" in resp.errorsByRow[0].errors[0]

    # One append, one delete (only the successful one deletes from trash)
    assert len(sheets.append_calls) == 1
    assert len(sheets.delete_calls) == 1

    # IMPORTANT: no change-date log when errors exist (matching TS behavior)
    assert sheets.batch_updates == []
