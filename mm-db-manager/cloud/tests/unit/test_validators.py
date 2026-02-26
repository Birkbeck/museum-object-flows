from __future__ import annotations

from mm_db_cloud.utils.validators import validate_form_row
from mm_db_cloud.config.sheet_config import Add, Edit


def _blank_row(n: int) -> list[str]:
    return [""] * n


def test_validate_form_row_requires_name_and_valid_postcode():
    # Minimal row with empty name and bad postcode.
    row = _blank_row(Add.NOTES + 1)
    row[Add.MUSEUM_NAME] = ""  # invalid
    row[Add.POSTCODE] = "NOT A POSTCODE"  # invalid
    row[Add.ACCREDITATION] = "accredited"
    row[Add.GOVERNANCE] = "local authority"
    row[Add.SIZE] = "small"
    row[Add.SUBJECT] = "mixed"
    row[Add.YEAR_OPENED] = "1999"
    row[Add.YEAR_CLOSED] = "2000"

    errs = validate_form_row(row, Add)
    assert "Museum must have a name." in errs
    assert any("Postcode" in e for e in errs)


def test_validate_form_row_wikidata_if_present_must_match_pattern():
    row = _blank_row(Add.NOTES + 1)
    row[Add.MUSEUM_NAME] = "Museum"
    row[Add.POSTCODE] = "WC1E 7HZ"
    row[Add.ACCREDITATION] = "accredited"
    row[Add.GOVERNANCE] = "local authority"
    row[Add.SIZE] = "small"
    row[Add.SUBJECT] = "mixed"
    row[Add.YEAR_OPENED] = "1999"
    row[Add.YEAR_CLOSED] = "2000"

    row[Add.WIKIDATA_ID] = "ABC"  # invalid
    errs = validate_form_row(row, Add)
    assert any("Wikidata ID" in e for e in errs)

    row[Add.WIKIDATA_ID] = "Q56"  # valid
    errs2 = validate_form_row(row, Add)
    assert not any("Wikidata ID" in e for e in errs2)


def test_validate_form_row_year_ranges_must_be_valid():
    row = _blank_row(Add.NOTES + 1)
    row[Add.MUSEUM_NAME] = "Museum"
    row[Add.POSTCODE] = "WC1E 7HZ"
    row[Add.ACCREDITATION] = "accredited"
    row[Add.GOVERNANCE] = "local authority"
    row[Add.SIZE] = "small"
    row[Add.SUBJECT] = "mixed"

    row[Add.YEAR_OPENED] = "2001/1999"  # invalid (start > end)
    row[Add.YEAR_CLOSED] = "2000"
    errs = validate_form_row(row, Add)
    assert any("Year opened" in e for e in errs)

    row[Add.YEAR_OPENED] = "1999/2001"  # valid
    errs2 = validate_form_row(row, Add)
    assert not any("Year opened" in e for e in errs2)


def test_validate_form_row_works_with_edit_sheet_indices_too():
    row = _blank_row(Edit.NOTES + 1)

    row[Edit.MUSEUM_NAME] = "Museum"
    row[Edit.POSTCODE] = "WC1E 7HZ"
    row[Edit.ACCREDITATION] = "accredited"
    row[Edit.GOVERNANCE] = "local authority"
    row[Edit.SIZE] = "small"
    row[Edit.SUBJECT] = "mixed"
    row[Edit.YEAR_OPENED] = "1999"
    row[Edit.YEAR_CLOSED] = "2000"
    row[Edit.WIKIDATA_ID] = "Q56"

    errs = validate_form_row(row, Edit)
    assert errs == []
