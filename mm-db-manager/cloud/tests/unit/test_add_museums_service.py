from __future__ import annotations

from typing import Any, Dict, List, Tuple

from mm_db_cloud.models.add_museums import AddMuseumsRequest
from mm_db_cloud.services.add_museums_service import AddMuseumsService
from mm_db_cloud.config.sheet_config import Add, Database, Instructions


class FakeSheetsService:
    def __init__(self) -> None:
        self.values: Dict[Tuple[str, str], List[List[Any]]] = {}
        self.sheet_ids: Dict[Tuple[str, str], int] = {}
        self.append_calls: List[Tuple[str, str, List[Any]]] = []
        self.delete_calls: List[Tuple[str, int, int, int]] = []
        self.batch_updates: List[Tuple[str, List[Tuple[str, List[List[Any]]]]]] = []

    def get_values(self, spreadsheet_id: str, range_a1: str) -> List[List[Any]]:
        return self.values.get((spreadsheet_id, range_a1), [])

    def append_row(
        self, spreadsheet_id: str, sheet_name: str, row_values: List[Any]
    ) -> Dict[str, Any]:
        self.append_calls.append((spreadsheet_id, sheet_name, row_values))
        return {"ok": True}

    def get_sheet_id_by_name(self, spreadsheet_id: str, sheet_name: str) -> int:
        return self.sheet_ids[(spreadsheet_id, sheet_name)]

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


def test_add_museums_happy_path_appends_and_deletes_bottom_up(monkeypatch):
    ssid = "S1"
    sheets = FakeSheetsService()

    sheets.values[(ssid, f"{Add.SHEET_NAME}!A2:Z")] = [
        [
            True,  # ready
            "Museum A",  # name
            "A Museum",  # alternative name
            "Q1",  # wikidata id
            "123 Malet Street",  # address 1
            "Bloomsbury",  # address 2
            "Camden",  # address 3
            "London",  # village town city
            "WC1E 7HZ",  # postcode
            "accredited",  # accreditation
            "123",  # accreditation number
            "",  # date accreditation changes
            "local authority",  # governance
            "governance source",  # governance source
            "independent: not-for-profit",  # previous governance
            "1999",  # prev gov start
            "2000",  # prev gov end
            "small",  # size
            "size source",  # size source
            "mixed",  # subject
            "1999",  # year opened
            "year opened source",  # year opened source
            "2001",  # year closed
            "year closed source",  # year closed source
            "source",  # primary provenance of data
            "some notes",  # notes
        ],
        [
            True,  # ready
            "Museum B",  # name
            "",  # alternative name
            "Q2",  # wikidata id
            "",  # address 1
            "",  # address 2
            "",  # address 3
            "",  # village town city
            "WC1E 7HZ",  # postcode
            "unaccredited",  # accreditation
            "",  # accreditation number
            "",  # date accreditation changed
            "local authority",  # governance
            "",  # governance source
            "",  # previous governance
            "",  # prev gov start
            "",  # prev gov end
            "small",  # size
            "size source",  # size source
            "mixed",  # subject
            "1999",  # year opened
            "year opened source",  # year opened source
            "2000",  # year closed
            "year closed source",  # year closed source
            "source",  # primary provenance of data
            "some notes",  # notes
        ],
    ]
    sheets.sheet_ids[(ssid, Add.SHEET_NAME)] = 123

    # Patch validator to accept both rows
    monkeypatch.setattr(
        "mm_db_cloud.services.add_museums_service.validate_form_row",
        lambda row, sheet_cls: [],
    )

    # Patch allocator to return deterministic ids
    monkeypatch.setattr(
        "mm_db_cloud.services.add_museums_service.IdAllocator.allocate_next_ids",
        lambda self, spreadsheet_id, count: [f"mm.new.{i+1}" for i in range(count)],
    )

    svc = AddMuseumsService(sheets)
    resp = svc.run(AddMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.addedCount == 2
    assert resp.skippedNotReady == 0
    assert resp.errorsByRow == []

    # Appended twice to DB
    assert len(sheets.append_calls) == 2
    assert sheets.append_calls[0][1] == Database.SHEET_NAME
    assert sheets.append_calls[1][1] == Database.SHEET_NAME

    # Deletes bottom-up: sheet row numbers are 2 and 3 -> 0-based [2,3) then [1,2)
    # Wait: rows in A2:Z correspond to sheet rows 2 and 3.
    assert sheets.delete_calls == [
        (ssid, 123, 2, 3),  # delete sheet row 3
        (ssid, 123, 1, 2),  # delete sheet row 2
    ]

    # Instructions timestamp updated once
    assert any(
        updates and updates[0][0] == f"{Instructions.SHEET_NAME}!{Instructions.DATE_A1}"
        for _sid, updates in sheets.batch_updates
    )


def test_add_museums_skips_not_ready(monkeypatch):
    ssid = "S1"
    sheets = FakeSheetsService()
    sheets.values[(ssid, f"{Add.SHEET_NAME}!A2:Z")] = [
        [False, "Museum A"],
        [
            True,  # ready
            "Museum B",  # name
            "",  # alternative name
            "Q2",  # wikidata id
            "",  # address 1
            "",  # address 2
            "",  # address 3
            "",  # village town city
            "WC1E 7HZ",  # postcode
            "unaccredited",  # accreditation
            "",  # accreditation number
            "",  # date accreditation changed
            "local authority",  # governance
            "",  # governance source
            "",  # previous governance
            "",  # prev gov start
            "",  # prev gov end
            "small",  # size
            "size source",  # size source
            "mixed",  # subject
            "1999",  # year opened
            "year opened source",  # year opened source
            "2000",  # year closed
            "year closed source",  # year closed source
            "source",  # primary provenance of data
            "some notes",  # notes
        ],
    ]
    sheets.sheet_ids[(ssid, Add.SHEET_NAME)] = 123

    monkeypatch.setattr(
        "mm_db_cloud.services.add_museums_service.validate_form_row",
        lambda row, sheet_cls: [],
    )
    monkeypatch.setattr(
        "mm_db_cloud.services.add_museums_service.IdAllocator.allocate_next_ids",
        lambda self, spreadsheet_id, count: ["mm.new.1"],
    )

    svc = AddMuseumsService(sheets)
    resp = svc.run(AddMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.addedCount == 1
    assert resp.skippedNotReady == 1
    assert len(sheets.append_calls) == 1
    assert len(sheets.delete_calls) == 1


def test_add_museums_validation_error_reports_and_does_not_write(monkeypatch):
    ssid = "S1"
    sheets = FakeSheetsService()
    sheets.values[(ssid, f"{Add.SHEET_NAME}!A2:Z")] = [[True, "Museum A"]]
    sheets.sheet_ids[(ssid, Add.SHEET_NAME)] = 123

    monkeypatch.setattr(
        "mm_db_cloud.services.add_museums_service.validate_form_row",
        lambda row, sheet_cls: ["bad row"],
    )

    svc = AddMuseumsService(sheets)
    resp = svc.run(AddMuseumsRequest(), ssid)

    assert resp.ok is True
    assert resp.addedCount == 0
    assert len(resp.errorsByRow) == 1
    assert sheets.append_calls == []
    assert sheets.delete_calls == []
