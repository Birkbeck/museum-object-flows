import base64
import hashlib
import hmac
import json
import os
import time
import uuid
import urllib.error
import urllib.request
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple

import pytest

import google.auth
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError


# ----------------------------
# Config + skipping
# ----------------------------

REQUIRED_ENV = [
    "MM_DB_CLOUD_BASE_URL",
    "MM_DB_CLOUD_HMAC_SECRET",
    "MM_DB_TEST_SPREADSHEET_ID",
]

SHEETS_SCOPES = [
    "https://www.googleapis.com/auth/spreadsheets",
    "https://www.googleapis.com/auth/drive",
]


def _missing_env() -> List[str]:
    return [k for k in REQUIRED_ENV if not os.environ.get(k)]


pytestmark = pytest.mark.skipif(
    bool(_missing_env()),
    reason="Missing env vars: " + ", ".join(_missing_env()),
)


@dataclass(frozen=True)
class Config:
    base_url: str
    hmac_secret: str
    spreadsheet_id: str


def _cfg() -> Config:
    return Config(
        base_url=os.environ["MM_DB_CLOUD_BASE_URL"].rstrip("/"),
        hmac_secret=os.environ["MM_DB_CLOUD_HMAC_SECRET"],
        spreadsheet_id=os.environ["MM_DB_TEST_SPREADSHEET_ID"],
    )


# ----------------------------
# Helpers: HTTP (HMAC signed)
# ----------------------------


def _sign_hmac_b64(secret: str, raw_body: bytes) -> str:
    mac = hmac.new(secret.encode("utf-8"), raw_body, hashlib.sha256).digest()
    return base64.b64encode(mac).decode("utf-8")


def post_json_signed(
    url: str, payload: Dict[str, Any], hmac_secret: str, timeout_s: int = 30
) -> Dict[str, Any]:
    raw = json.dumps(payload).encode("utf-8")
    sig = _sign_hmac_b64(hmac_secret, raw)

    req = urllib.request.Request(
        url=url,
        data=raw,
        method="POST",
        headers={
            "Content-Type": "application/json",
            "X-Signature": sig,
        },
    )

    try:
        with urllib.request.urlopen(req, timeout=timeout_s) as resp:
            body = resp.read().decode("utf-8")
            data = json.loads(body)
            if data.get("ok") is True:
                return data
            raise RuntimeError(f"Unexpected response schema: {data}")
    except urllib.error.HTTPError as e:
        body = e.read().decode("utf-8", errors="replace")
        raise RuntimeError(f"HTTP {e.code} from {url}: {body}") from e


# ----------------------------
# Helpers: Sheets API (write + verification)
# ----------------------------


def make_sheets_client():
    creds, _ = google.auth.default(scopes=SHEETS_SCOPES)
    return build("sheets", "v4", credentials=creds, cache_discovery=False)


def read_range(service, spreadsheet_id: str, range_a1: str) -> List[List[Any]]:
    resp = (
        service.spreadsheets()
        .values()
        .get(spreadsheetId=spreadsheet_id, range=range_a1)
        .execute()
    )
    return resp.get("values", [])


def read_cell(service, spreadsheet_id: str, range_a1: str) -> Any:
    vals = read_range(service, spreadsheet_id, range_a1)
    if not vals or not vals[0]:
        return ""
    return vals[0][0]


def append_row(
    service, spreadsheet_id: str, sheet_name: str, row_values: List[Any]
) -> None:
    (
        service.spreadsheets()
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


def update_cell(service, spreadsheet_id: str, range_a1: str, value: Any) -> None:
    (
        service.spreadsheets()
        .values()
        .update(
            spreadsheetId=spreadsheet_id,
            range=range_a1,
            valueInputOption="RAW",
            body={"values": [[value]]},
        )
        .execute()
    )


def get_last_row(
    service, spreadsheet_id: str, sheet_name: str, max_scan_rows: int = 5000
) -> int:
    """
    Approximate last row by reading a bounded column and finding last non-empty.
    Bounded to avoid huge reads.
    """
    values = read_range(service, spreadsheet_id, f"{sheet_name}!A1:A{max_scan_rows}")
    last = 0
    for i, row in enumerate(values, start=1):
        if row and str(row[0]).strip() != "":
            last = i
    return last


def find_row_by_exact_id_bounded(
    service,
    spreadsheet_id: str,
    sheet_name: str,
    museum_id: str,
    *,
    id_col: str = "A",
    start_row: int = 1,
    end_row: int = 3000,
) -> Optional[int]:
    """
    Returns 1-based row number where <id_col> == museum_id, else None.
    Uses a bounded range to avoid reading full columns.
    """
    values = read_range(
        service, spreadsheet_id, f"{sheet_name}!{id_col}{start_row}:{id_col}{end_row}"
    )
    for offset, row in enumerate(values):
        if row and row[0] == museum_id:
            return start_row + offset
    return None


def find_row_by_marker_bounded(
    service,
    spreadsheet_id: str,
    sheet_name: str,
    marker: str,
    *,
    start_row: int = 1,
    end_row: int = 2000,
    start_col: str = "A",
    end_col: str = "AF",
) -> Optional[int]:
    """
    Returns 1-based row number where marker appears anywhere in the row.
    Uses a bounded range (much cheaper than A:AF without row limits).
    """
    values = read_range(
        service,
        spreadsheet_id,
        f"{sheet_name}!{start_col}{start_row}:{end_col}{end_row}",
    )
    for offset, row in enumerate(values):
        if any(cell == marker for cell in row):
            return start_row + offset
    return None


def wait_until_present(
    fn,
    *,
    timeout_s: int = 45,
    initial_sleep_s: float = 1.0,
    max_sleep_s: float = 6.0,
):
    """
    Exponential backoff poll. Returns first non-None value.
    Handles Sheets 429 by backing off more.
    """
    deadline = time.time() + timeout_s
    sleep_s = initial_sleep_s
    while time.time() < deadline:
        try:
            v = fn()
        except HttpError as e:
            if getattr(e, "resp", None) is not None and e.resp.status == 429:
                time.sleep(min(max_sleep_s, max(3.0, sleep_s)))
                sleep_s = min(sleep_s * 1.8, max_sleep_s)
                continue
            raise

        if v is not None:
            return v

        time.sleep(sleep_s)
        sleep_s = min(sleep_s * 1.6, max_sleep_s)

    raise AssertionError("Timed out waiting for value to become present")


def wait_until_absent(
    fn,
    *,
    timeout_s: int = 45,
    initial_sleep_s: float = 1.0,
    max_sleep_s: float = 6.0,
):
    """
    Exponential backoff poll. Succeeds when fn() returns None.
    Handles Sheets 429 by backing off more.
    """
    deadline = time.time() + timeout_s
    sleep_s = initial_sleep_s
    while time.time() < deadline:
        try:
            v = fn()
        except HttpError as e:
            if getattr(e, "resp", None) is not None and e.resp.status == 429:
                time.sleep(min(max_sleep_s, max(3.0, sleep_s)))
                sleep_s = min(sleep_s * 1.8, max_sleep_s)
                continue
            raise

        if v is None:
            return

        time.sleep(sleep_s)
        sleep_s = min(sleep_s * 1.6, max_sleep_s)

    raise AssertionError("Timed out waiting for value to become absent")


def wait_for_row_present(
    service, spreadsheet_id: str, sheet_name: str, marker: str, timeout_s: int = 45
) -> int:
    return wait_until_present(
        lambda: find_row_by_marker_bounded(
            service, spreadsheet_id, sheet_name, marker, start_row=1, end_row=2000
        ),
        timeout_s=timeout_s,
    )


def wait_for_row_absent(
    service, spreadsheet_id: str, sheet_name: str, marker: str, timeout_s: int = 45
) -> None:
    wait_until_absent(
        lambda: find_row_by_marker_bounded(
            service, spreadsheet_id, sheet_name, marker, start_row=1, end_row=2000
        ),
        timeout_s=timeout_s,
    )


def read_db_id_and_name(
    service, spreadsheet_id: str, db_sheet_name: str, row_number_1_based: int
) -> Tuple[str, str]:
    vals = read_range(
        service,
        spreadsheet_id,
        f"{db_sheet_name}!A{row_number_1_based}:B{row_number_1_based}",
    )
    if not vals or not vals[0] or len(vals[0]) < 2:
        raise AssertionError("Could not read DB ID+Name")
    return str(vals[0][0]), str(vals[0][1])


# ----------------------------
# Sheet names (must match your config/sheet_config.py)
# ----------------------------

INSTRUCTIONS_SHEET = "Instructions"
DB_SHEET = "Database (read only)"
ADD_SHEET = "Add"
EDIT_SHEET = "Edit"
DELETE_SHEET = "Delete"
TRASH_SHEET = "Trash"


# ----------------------------
# Build form rows matching your validators
# ----------------------------


def build_add_row(*, museum_name: str) -> List[Any]:
    """
    Matches Add sheet columns (0..25):
      0 READY_TO_COMMIT
      1 MUSEUM_NAME
      2 ALTERNATIVE_NAME
      3 WIKIDATA_ID
      4 ADDRESS_1
      5 ADDRESS_2
      6 ADDRESS_3
      7 VILLAGE_TOWN_CITY
      8 POSTCODE
      9 ACCREDITATION
      10 ACCREDITATION_NUMBER
      11 ACCREDITATION_CHANGE_DATE
      12 GOVERNANCE
      13 GOVERNANCE_SOURCE
      14 PREVIOUS_GOVERNANCE
      15 PREVIOUS_GOVERNANCE_START
      16 PREVIOUS_GOVERNANCE_END
      17 SIZE
      18 SIZE_SOURCE
      19 SUBJECT
      20 YEAR_OPENED
      21 YEAR_OPENED_SOURCE
      22 YEAR_CLOSED
      23 YEAR_CLOSED_SOURCE
      24 PRIMARY_PROVENANCE_OF_DATA
      25 NOTES
    """
    return [
        True,
        museum_name,
        "",
        "Q56",
        "1 Example Street",
        "",
        "",
        "London",
        "WC1E 7HZ",
        "accredited",
        123,
        "2020",
        "local authority",
        "integration-test",
        "",
        "",
        "",
        "small",
        "integration-test",
        "arts",
        "1990",
        "integration-test",
        "1991",
        "integration-test",
        "integration-test",
        "integration-test",
    ]


def build_edit_row(*, museum_id: str, old_name: str, new_name: str) -> List[Any]:
    """
    Matches Edit sheet columns (0..26):
      0 READY_TO_COMMIT
      1 MUSEUM ("id - name")
      2 MUSEUM_NAME
      3 ALTERNATIVE_NAME
      4 WIKIDATA_ID
      5 ADDRESS_1
      6 ADDRESS_2
      7 ADDRESS_3
      8 VILLAGE_TOWN_CITY
      9 POSTCODE
      10 ACCREDITATION
      11 ACCREDITATION_NUMBER
      12 ACCREDITATION_CHANGE_DATE
      13 GOVERNANCE
      14 GOVERNANCE_SOURCE
      15 PREVIOUS_GOVERNANCE
      16 PREVIOUS_GOVERNANCE_START
      17 PREVIOUS_GOVERNANCE_END
      18 SIZE
      19 SIZE_SOURCE
      20 SUBJECT
      21 YEAR_OPENED
      22 YEAR_OPENED_SOURCE
      23 YEAR_CLOSED
      24 YEAR_CLOSED_SOURCE
      25 PRIMARY_PROVENANCE_OF_DATA
      26 NOTES
    """
    return [
        True,
        f"{museum_id} - {old_name}",
        new_name,
        "",
        "Q56",
        "1 Example Street",
        "",
        "",
        "London",
        "WC1E 7HZ",
        "accredited",
        123,
        "2020",
        "local authority",
        "integration-test",
        "",
        "",
        "",
        "small",
        "integration-test",
        "arts",
        "1990",
        "integration-test",
        "1991",
        "integration-test",
        "integration-test",
        "integration-test",
    ]


def build_delete_row(*, museum_id: str, name: str) -> List[Any]:
    """
    We only need columns:
      0 READY_TO_DELETE (bool)
      1 MUSEUM ("id - name")
    """
    return [True, f"{museum_id} - {name}"]


# ----------------------------
# The integration test (domain endpoints)
# ----------------------------


@pytest.mark.integration
def test_full_row_lifecycle_real_sheet_domain_endpoints():
    cfg = _cfg()
    sheets = make_sheets_client()

    marker_v1 = f"ITEST-{uuid.uuid4()}"
    marker_v2 = marker_v1 + "-EDITED"

    # ---- 1) Stage Add row in Add sheet, then call addMuseums ----
    append_row(
        sheets, cfg.spreadsheet_id, ADD_SHEET, build_add_row(museum_name=marker_v1)
    )

    post_json_signed(
        f"{cfg.base_url}/addMuseums",
        {},
        cfg.hmac_secret,
    )

    db_row = wait_for_row_present(sheets, cfg.spreadsheet_id, DB_SHEET, marker_v1)
    museum_id, museum_name = read_db_id_and_name(
        sheets, cfg.spreadsheet_id, DB_SHEET, db_row
    )
    assert museum_name == marker_v1
    assert museum_id.startswith("mm.new.")

    # ---- 2) Stage Edit row in Edit sheet, then call editMuseums ----
    append_row(
        sheets,
        cfg.spreadsheet_id,
        EDIT_SHEET,
        build_edit_row(museum_id=museum_id, old_name=marker_v1, new_name=marker_v2),
    )

    post_json_signed(
        f"{cfg.base_url}/editMuseums",
        {},
        cfg.hmac_secret,
    )

    wait_for_row_absent(sheets, cfg.spreadsheet_id, DB_SHEET, marker_v1)
    db_row_after_edit = wait_for_row_present(
        sheets, cfg.spreadsheet_id, DB_SHEET, marker_v2
    )
    museum_id_after, museum_name_after = read_db_id_and_name(
        sheets, cfg.spreadsheet_id, DB_SHEET, db_row_after_edit
    )
    assert museum_id_after == museum_id
    assert museum_name_after == marker_v2

    # ---- 3) Stage Delete row in Delete sheet, then call trashMuseums ----
    append_row(
        sheets,
        cfg.spreadsheet_id,
        DELETE_SHEET,
        build_delete_row(museum_id=museum_id, name=marker_v2),
    )

    post_json_signed(
        f"{cfg.base_url}/trashMuseums",
        {},
        cfg.hmac_secret,
    )

    # Verify absent in DB by ID, present in Trash by ID
    assert (
        find_row_by_exact_id_bounded(
            sheets,
            cfg.spreadsheet_id,
            DB_SHEET,
            museum_id,
            id_col="A",
            start_row=1,
            end_row=3000,
        )
        is None
    )
    trash_row_1 = wait_for_row_present(
        sheets, cfg.spreadsheet_id, TRASH_SHEET, museum_id
    )
    assert trash_row_1 is not None

    # ---- 4) Mark RESTORE checkbox in Trash, then call restoreMuseums ----
    # Trash.RESTORE is column B (1-indexed => "B")
    update_cell(sheets, cfg.spreadsheet_id, f"{TRASH_SHEET}!B{trash_row_1}", True)

    post_json_signed(
        f"{cfg.base_url}/restoreMuseums",
        {},
        cfg.hmac_secret,
    )

    # back in DB, removed from Trash
    restored_db_row = wait_until_present(
        lambda: find_row_by_exact_id_bounded(
            sheets,
            cfg.spreadsheet_id,
            DB_SHEET,
            museum_id,
            id_col="A",
            start_row=1,
            end_row=3000,
        ),
        timeout_s=45,
    )
    if restored_db_row is None:
        raise AssertionError("Timed out waiting for restored row to appear in DB")

    wait_until_absent(
        lambda: find_row_by_exact_id_bounded(
            sheets,
            cfg.spreadsheet_id,
            TRASH_SHEET,
            museum_id,
            id_col="A",
            start_row=1,
            end_row=3000,
        ),
        timeout_s=45,
    )

    # ---- 5) Trash again: stage Delete row again, call trashMuseums ----
    append_row(
        sheets,
        cfg.spreadsheet_id,
        DELETE_SHEET,
        build_delete_row(museum_id=museum_id, name=marker_v2),
    )

    post_json_signed(
        f"{cfg.base_url}/trashMuseums",
        {},
        cfg.hmac_secret,
    )

    assert (
        find_row_by_exact_id_bounded(
            sheets,
            cfg.spreadsheet_id,
            DB_SHEET,
            museum_id,
            id_col="A",
            start_row=1,
            end_row=3000,
        )
        is None
    )
    trash_row_2 = wait_for_row_present(
        sheets, cfg.spreadsheet_id, TRASH_SHEET, museum_id
    )

    # ---- 6) Mark PERMANENTLY_DELETE checkbox in Trash, then call permanentlyDeleteMuseums ----
    # Trash.PERMANENTLY_DELETE is column A
    update_cell(sheets, cfg.spreadsheet_id, f"{TRASH_SHEET}!A{trash_row_2}", True)

    post_json_signed(
        f"{cfg.base_url}/permanentlyDeleteMuseums",
        {},
        cfg.hmac_secret,
    )

    # verify gone from Trash
    wait_until_absent(
        lambda: find_row_by_exact_id_bounded(
            sheets,
            cfg.spreadsheet_id,
            TRASH_SHEET,
            museum_id,
            id_col="A",
            start_row=1,
            end_row=3000,
        ),
        timeout_s=60,
    )
