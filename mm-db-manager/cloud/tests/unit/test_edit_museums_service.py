from __future__ import annotations

from typing import Any, Dict, List, Tuple

from mm_db_cloud.models.edit_museums import EditMuseumsRequest
from mm_db_cloud.services.edit_museums_service import EditMuseumsService
from mm_db_cloud.config.sheet_config import Edit, Database, Instructions


class FakeSheetsService:
    def __init__(self) -> None:
        self.values: Dict[Tuple[str, str], List[List[Any]]] = {}
        self.sheet_ids: Dict[Tuple[str, str], int] = {}
        self.batch_updates: List[Tuple[str, List[Tuple[str, List[List[Any]]]]]] = []
        self.delete_calls: List[Tuple[str, int, int, int]] = []

    def get_values(self, spreadsheet_id: str, range_a1: str) -> List[List[Any]]:
        return self.values.get((spreadsheet_id, range_a1), [])

    def get_sheet_id_by_name(self, spreadsheet_id: str, sheet_name: str) -> int:
        return self.sheet_ids[(spreadsheet_id, sheet_name)]

    def batch_update_values(
        self, spreadsheet_id: str, updates: List[Tuple[str, List[List[Any]]]]
    ) -> Dict[str, Any]:
        self.batch_updates.append((spreadsheet_id, updates))
        return {"ok": True}

    def delete_rows(
        self, spreadsheet_id: str, sheet_id: int, start_index: int, end_index: int
    ) -> Dict[str, Any]:
        self.delete_calls.append((spreadsheet_id, sheet_id, start_index, end_index))
        return {"ok": True}


def test_edit_museums_happy_path_updates_and_deletes_bottom_up(monkeypatch):
    ssid = "S1"
    sheets = FakeSheetsService()

    sheets.values[(ssid, f"{Edit.SHEET_NAME}!A2:ZZ")] = [
        [
            True,
            "mm.new.1 - Museum One",
            "New Name 1",  # name
            "Alt Name 1",  # alternative name
            "",  # wikidata id
            "",  # address 1
            "",  # address 2
            "",  # address 3
            "",  # village town city
            "",  # postcode
            "unaccredited",  # accreditation
            "",  # accreditation number
            "",  # data accreditation changed
            "local authority",  # governance
            "",  # governance source
            "",  # previous governance
            "",  # prev gov start
            "",  # prev gov end
            "small",  # size
            "",  # size source
            "local histories",  # subject
            "2001/2002",  # year opened
            "",  # year opened source
            "9999",  # year closed
            "",  # year closed source
        ],
        [
            True,
            "mm.new.2 - Museum Two",
            "New Name 2",
            "Alt Name 2",
            "",  # wikidata id
            "",  # address 1
            "",  # address 2
            "",  # address 3
            "",  # village town city
            "",  # postcode
            "unaccredited",  # accreditation
            "",  # accreditation number
            "",  # data accreditation changed
            "local authority",  # governance
            "",  # governance source
            "",  # previous governance
            "",  # prev gov start
            "",  # prev gov end
            "small",  # size
            "",  # size source
            "local histories",  # subject
            "2001/2002",  # year opened
            "",  # year opened source
            "9999",  # year closed
            "",  # year closed source
        ],
    ]
    sheets.sheet_ids[(ssid, Edit.SHEET_NAME)] = 777

    # DB id map read
    sheets.values[(ssid, f"{Database.SHEET_NAME}!A2:A")] = [["mm.new.1"], ["mm.new.2"]]

    monkeypatch.setattr(
        "mm_db_cloud.services.edit_museums_service.validate_form_row",
        lambda row, sheet_cls: [],
    )

    svc = EditMuseumsService(sheets)
    resp = svc.run(EditMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.editedCount == 2
    assert resp.skippedNotReady == 0
    assert resp.errorsByRow == []

    # Two DB writes + one instructions write (since no errors)
    db_updates = [
        upd
        for _sid, updates in sheets.batch_updates
        for upd in updates
        if upd[0].startswith(f"{Database.SHEET_NAME}!")
    ]
    assert len(db_updates) == 2

    # Deletes bottom-up: edit sheet rows 3 then 2
    assert sheets.delete_calls == [
        (ssid, 777, 2, 3),
        (ssid, 777, 1, 2),
    ]

    assert any(
        updates and updates[0][0] == f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}"
        for _sid, updates in sheets.batch_updates
    )


def test_edit_museums_missing_db_id_reports_error_and_does_not_delete(monkeypatch):
    ssid = "S1"
    sheets = FakeSheetsService()

    sheets.values[(ssid, f"{Edit.SHEET_NAME}!A2:ZZ")] = [
        [True, "mm.new.999 - Missing", "New Name"],
    ]
    sheets.sheet_ids[(ssid, Edit.SHEET_NAME)] = 777
    sheets.values[(ssid, f"{Database.SHEET_NAME}!A2:A")] = [["mm.new.1"]]

    monkeypatch.setattr(
        "mm_db_cloud.services.edit_museums_service.validate_form_row",
        lambda row, sheet_cls: [],
    )

    svc = EditMuseumsService(sheets)
    resp = svc.run(EditMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.editedCount == 0
    assert len(resp.errorsByRow) == 1
    assert "not found" in resp.errorsByRow[0].errors[0]
    assert sheets.delete_calls == []

    # no instructions log because there was an error
    assert not any(
        updates and updates[0][0] == f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}"
        for _sid, updates in sheets.batch_updates
    )


def test_edit_museums_invalid_museum_cell_errors(monkeypatch):
    ssid = "S1"
    sheets = FakeSheetsService()

    sheets.values[(ssid, f"{Edit.SHEET_NAME}!A2:ZZ")] = [
        [True, "mm.new.1", "New Name"],  # missing " - "
    ]
    sheets.sheet_ids[(ssid, Edit.SHEET_NAME)] = 777
    sheets.values[(ssid, f"{Database.SHEET_NAME}!A2:A")] = [["mm.new.1"]]

    monkeypatch.setattr(
        "mm_db_cloud.services.edit_museums_service.validate_form_row",
        lambda row, sheet_cls: [],
    )

    svc = EditMuseumsService(sheets)
    resp = svc.run(EditMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.editedCount == 0
    assert len(resp.errorsByRow) == 1
    assert 'Expected "id - name"' in resp.errorsByRow[0].errors[0]
    assert sheets.delete_calls == []
