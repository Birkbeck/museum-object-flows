from __future__ import annotations

from typing import Any, Dict, List, Optional, Tuple

import google.auth
from google.auth.credentials import with_scopes_if_required
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError


SHEETS_SCOPES = [
    "https://www.googleapis.com/auth/spreadsheets",
    "https://www.googleapis.com/auth/drive",
]


class SheetsService:
    def __init__(self) -> None:
        creds, _ = google.auth.default()
        creds = with_scopes_if_required(creds, scopes=SHEETS_SCOPES)
        self._service = build("sheets", "v4", credentials=creds, cache_discovery=False)

    def col_to_a1(self, col_1_indexed: int) -> str:
        """
        Convert a 1-indexed column number to A1 column letters.
        1->A, 2->B, ... 26->Z, 27->AA, etc.
        """
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
        start_row = header_row_1_indexed + 1
        col_letter = self.col_to_a1(col_1_indexed)
        range_a1 = f"{sheet_name}!{col_letter}{start_row}:{col_letter}{start_row + scan_limit - 1}"
        vals = self.get_values(spreadsheet_id, range_a1)

        for i, row in enumerate(vals):
            v = row[0] if row else ""
            if str(v or "").strip() == "":
                return start_row + i

        return start_row + len(vals)

    def append_row(
        self, spreadsheet_id: str, sheet_name: str, row_values: List[Any]
    ) -> Dict[str, Any]:
        return (
            self._service.spreadsheets()
            .values()
            .append(
                spreadsheetId=spreadsheet_id,
                range=f"{sheet_name}!A:A",
                valueInputOption="RAW",
                insertDataOption="INSERT_ROWS",
                body={"values": [row_values]},
            )
            .execute()
        )

    def batch_update_values(
        self, spreadsheet_id: str, updates: List[Tuple[str, List[List[Any]]]]
    ) -> Dict[str, Any]:
        data = [{"range": r, "values": v} for (r, v) in updates]
        return (
            self._service.spreadsheets()
            .values()
            .batchUpdate(
                spreadsheetId=spreadsheet_id,
                body={
                    "valueInputOption": "RAW",
                    "data": data,
                },
            )
            .execute()
        )

    def clear_range(self, spreadsheet_id: str, range_a1: str) -> Dict[str, Any]:
        return (
            self._service.spreadsheets()
            .values()
            .clear(spreadsheetId=spreadsheet_id, range=range_a1, body={})
            .execute()
        )

    def batch_update_structural(
        self, spreadsheet_id: str, requests: List[Dict[str, Any]]
    ) -> Dict[str, Any]:
        return (
            self._service.spreadsheets()
            .batchUpdate(spreadsheetId=spreadsheet_id, body={"requests": requests})
            .execute()
        )

    def get_values(self, spreadsheet_id: str, range_a1: str) -> List[List[Any]]:
        resp = (
            self._service.spreadsheets()
            .values()
            .get(spreadsheetId=spreadsheet_id, range=range_a1)
            .execute()
        )
        return resp.get("values", [])

    def get_sheet_id_by_name(self, spreadsheet_id: str, sheet_name: str) -> int:
        meta = self._service.spreadsheets().get(spreadsheetId=spreadsheet_id).execute()
        for s in meta.get("sheets", []):
            props = s.get("properties", {})
            if props.get("title") == sheet_name:
                sid = props.get("sheetId")
                if isinstance(sid, int):
                    return sid
        raise ValueError(f"Sheet not found: {sheet_name}")

    def delete_rows(
        self, spreadsheet_id: str, sheet_id: int, start_index: int, end_index: int
    ) -> Dict[str, Any]:
        """
        start_index/end_index are 0-based, end exclusive (Sheets API semantics).
        """
        req = {
            "deleteDimension": {
                "range": {
                    "sheetId": sheet_id,
                    "dimension": "ROWS",
                    "startIndex": start_index,
                    "endIndex": end_index,
                }
            }
        }
        return self.batch_update_structural(spreadsheet_id, [req])

    def create_sheet(self, spreadsheet_id: str, title: str) -> Dict[str, Any]:
        req = {"addSheet": {"properties": {"title": title}}}
        return self.batch_update_structural(spreadsheet_id, [req])

    def hide_sheet(self, spreadsheet_id: str, sheet_id: int) -> Dict[str, Any]:
        req = {
            "updateSheetProperties": {
                "properties": {"sheetId": sheet_id, "hidden": True},
                "fields": "hidden",
            }
        }
        return self.batch_update_structural(spreadsheet_id, [req])

    def get_single_value(self, spreadsheet_id: str, range_a1: str) -> Any:
        vals = self.get_values(spreadsheet_id, range_a1)
        if not vals or not vals[0]:
            return ""
        return vals[0][0]
