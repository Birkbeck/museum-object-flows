from __future__ import annotations

from typing import Any, List, Type

from mm_db_cloud.config.sheet_config import Database


def _get(values: List[Any], idx: int) -> Any:
    return values[idx] if idx < len(values) else ""


def split_year_range(value: Any) -> tuple[str, str]:
    if value is None:
        return ("", "")
    s = str(value).strip()
    if s == "":
        return ("", "")
    if "/" in s:
        a, b = s.split("/", 1)
        a = a.strip()
        b = b.strip()
        return (a, b if b else a)
    return (s, s)


def map_form_row_to_db_row(
    form_row: List[Any],
    museum_id: str,
    form_sheet_cls: Type[Any],
) -> List[Any]:
    """
    Map a form row (Add/Edit/Delete-style) into a full DB row.

    - `form_sheet_cls` is a config class like Add/Edit/Delete exposing column indices
      (MUSEUM_NAME, WIKIDATA_ID, POSTCODE, YEAR_OPENED, YEAR_CLOSED, etc.)
    - Broad fields are left blank (as in your previous mapper) unless you later
      define mapping rules.
    """
    out: List[Any] = [""] * (
        Database.NOTES + 1
    )  # DB_TOTAL_COLS inferred from last col index

    out[Database.ID] = museum_id
    out[Database.MUSEUM_NAME] = _get(form_row, form_sheet_cls.MUSEUM_NAME)
    out[Database.ALTERNATIVE_NAME] = _get(form_row, form_sheet_cls.ALTERNATIVE_NAME)
    out[Database.WIKIDATA_ID] = _get(form_row, form_sheet_cls.WIKIDATA_ID)

    out[Database.ADDRESS_1] = _get(form_row, form_sheet_cls.ADDRESS_1)
    out[Database.ADDRESS_2] = _get(form_row, form_sheet_cls.ADDRESS_2)
    out[Database.ADDRESS_3] = _get(form_row, form_sheet_cls.ADDRESS_3)
    out[Database.VILLAGE_TOWN_CITY] = _get(form_row, form_sheet_cls.VILLAGE_TOWN_CITY)
    out[Database.POSTCODE] = _get(form_row, form_sheet_cls.POSTCODE)

    out[Database.ACCREDITATION] = _get(form_row, form_sheet_cls.ACCREDITATION)
    out[Database.ACCREDITATION_NUMBER] = _get(
        form_row, form_sheet_cls.ACCREDITATION_NUMBER
    )
    out[Database.ACCREDITATION_CHANGE_DATE] = _get(
        form_row, form_sheet_cls.ACCREDITATION_CHANGE_DATE
    )

    governance = _get(form_row, form_sheet_cls.GOVERNANCE)
    out[Database.GOVERNANCE_BROAD] = governance.split(": ")[0]
    out[Database.GOVERNANCE] = (
        governance.split(": ")[1] if ": " in governance else governance
    )
    out[Database.GOVERNANCE_SOURCE] = _get(form_row, form_sheet_cls.GOVERNANCE_SOURCE)

    out[Database.PREVIOUS_GOVERNANCE] = _get(
        form_row, form_sheet_cls.PREVIOUS_GOVERNANCE
    )
    out[Database.PREVIOUS_GOVERNANCE_START] = _get(
        form_row, form_sheet_cls.PREVIOUS_GOVERNANCE_START
    )
    out[Database.PREVIOUS_GOVERNANCE_END] = _get(
        form_row, form_sheet_cls.PREVIOUS_GOVERNANCE_END
    )

    out[Database.SIZE] = _get(form_row, form_sheet_cls.SIZE)
    out[Database.SIZE_SOURCE] = _get(form_row, form_sheet_cls.SIZE_SOURCE)

    subject = _get(form_row, form_sheet_cls.SUBJECT)
    out[Database.SUBJECT_BROAD] = subject.split(":")[0]
    out[Database.SUBJECT] = subject

    y1, y2 = split_year_range(_get(form_row, form_sheet_cls.YEAR_OPENED))
    out[Database.YEAR_OPENED_1] = int(y1)
    out[Database.YEAR_OPENED_2] = int(y2)
    out[Database.YEAR_OPENED_SOURCE] = _get(form_row, form_sheet_cls.YEAR_OPENED_SOURCE)

    c1, c2 = split_year_range(_get(form_row, form_sheet_cls.YEAR_CLOSED))
    out[Database.YEAR_CLOSED_1] = int(c1)
    out[Database.YEAR_CLOSED_2] = int(c2)
    out[Database.YEAR_CLOSED_SOURCE] = _get(form_row, form_sheet_cls.YEAR_CLOSED_SOURCE)

    out[Database.PRIMARY_PROVENANCE_OF_DATA] = _get(
        form_row, form_sheet_cls.PRIMARY_PROVENANCE_OF_DATA
    )
    out[Database.NOTES] = _get(form_row, form_sheet_cls.NOTES)

    return out


def map_db_row_to_db_row(
    source_row: List[Any],
    *,
    source_sheet_cls: Type,
    dest_sheet_cls: Type,
) -> List[Any]:
    """
    Copy DB fields from source_sheet_cls into dest_sheet_cls by explicit field list.

    Intended for:
      - Database -> Trash
      - Trash -> Database
    """

    # Explicit DB field names only (NO metadata like HEADER_ROW / SHEET_NAME).
    fields = [
        "ID",
        "MUSEUM_NAME",
        "ALTERNATIVE_NAME",
        "WIKIDATA_ID",
        "ADDRESS_1",
        "ADDRESS_2",
        "ADDRESS_3",
        "VILLAGE_TOWN_CITY",
        "POSTCODE",
        "ACCREDITATION",
        "ACCREDITATION_NUMBER",
        "ACCREDITATION_CHANGE_DATE",
        "GOVERNANCE_BROAD",
        "GOVERNANCE",
        "GOVERNANCE_SOURCE",
        "PREVIOUS_GOVERNANCE",
        "PREVIOUS_GOVERNANCE_START",
        "PREVIOUS_GOVERNANCE_END",
        "SIZE",
        "SIZE_SOURCE",
        "SUBJECT_BROAD",
        "SUBJECT",
        "YEAR_OPENED_1",
        "YEAR_OPENED_2",
        "YEAR_OPENED_SOURCE",
        "YEAR_CLOSED_1",
        "YEAR_CLOSED_2",
        "YEAR_CLOSED_SOURCE",
        "PRIMARY_PROVENANCE_OF_DATA",
        "NOTES",
    ]

    out: List[Any] = [""] * dest_sheet_cls.TOTAL_COLS

    for name in fields:
        src_idx = getattr(source_sheet_cls, name)
        dest_idx = getattr(dest_sheet_cls, name)
        out[dest_idx] = _get(source_row, src_idx)

    return out
