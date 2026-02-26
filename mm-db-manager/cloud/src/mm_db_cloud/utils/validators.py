from __future__ import annotations

import os
import re
from typing import Any, List, Optional, Set, Type


from mm_db_cloud.config.allowed_values import (
    ACCREDITATION_VALUES,
    GOVERNANCE_VALUES,
    SIZE_VALUES,
    SUBJECT_VALUES,
)

_WIKIDATA_RE = re.compile(r"^Q\d+$")
# Very close to your TS regex; accepts common UK postcode formatting.
_POSTCODE_RE = re.compile(r"^[A-Z]{1,2}\d[A-Z\d]?\s+\d[A-Z]{2}$", re.IGNORECASE)
_YEAR_RANGE_RE = re.compile(r"^\d{4}(?:/\d{4})?$")


def _csv_env_set(name: str) -> Optional[Set[str]]:
    raw = os.environ.get(name)
    if not raw:
        return None
    return {x.strip() for x in raw.split(",") if x.strip()}


def is_empty(v: Any) -> bool:
    return v is None or str(v).strip() == ""


def is_not_empty(v: Any) -> bool:
    return not is_empty(v)


def get_cell(values: List[Any], idx: int) -> Any:
    return values[idx] if idx < len(values) else ""


def is_valid_wikidata_id(v: Any) -> bool:
    if not isinstance(v, str):
        return False
    return bool(_WIKIDATA_RE.match(v.strip()))


def is_valid_postcode(v: Any) -> bool:
    if not isinstance(v, str):
        return False
    return bool(_POSTCODE_RE.match(v.strip()))


def is_valid_year_range(v: Any) -> bool:
    if not isinstance(v, (str, int, float)):
        return False
    s = str(v).strip()
    if not _YEAR_RANGE_RE.match(s):
        return False
    start_s, *rest = s.split("/")
    end_s = rest[0] if rest else start_s
    try:
        start = int(start_s)
        end = int(end_s)
    except ValueError:
        return False
    return start <= end


def is_valid_accreditation_number(v: Any) -> bool:
    if v is None or v == "":
        return False
    try:
        num = int(v) if not isinstance(v, bool) else -1
    except Exception:
        try:
            num = int(float(v))
        except Exception:
            return False
    return num > 0


def _in_allowed_set_or_permissive(v: Any, allowed: Optional[Set[str]]) -> bool:
    if not isinstance(v, str):
        return False
    s = v.strip()
    if s == "":
        return False
    if allowed is None:
        # permissive fallback until you wire in the actual sets
        return True
    return s in allowed


def _in_allowed_set(value: Any, allowed: frozenset[str]) -> bool:
    return isinstance(value, str) and value in allowed


def validate_form_row(values: List[Any], sheet_cls: Type[Any]) -> List[str]:
    """
    Generic validator for Add/Edit/Delete form rows.

    `sheet_cls` is a config class (e.g. Add, Edit, Delete) that exposes
    integer column indices as class attributes.
    """
    errors: List[str] = []

    name = get_cell(values, sheet_cls.MUSEUM_NAME)
    if is_empty(name):
        errors.append("Museum must have a name.")

    wikidata = get_cell(values, sheet_cls.WIKIDATA_ID)
    if is_not_empty(wikidata) and not is_valid_wikidata_id(wikidata):
        errors.append(f"Wikidata ID {wikidata} is not a valid Wikidata ID.")

    postcode = get_cell(values, sheet_cls.POSTCODE)
    if not is_valid_postcode(postcode):
        errors.append(f"Postcode {postcode} is not a correctly formatted UK postcode.")

    accreditation = get_cell(values, sheet_cls.ACCREDITATION)
    if not _in_allowed_set(accreditation, ACCREDITATION_VALUES):
        errors.append(
            f"Accreditation {accreditation} is not a valid accreditation status."
        )

    acc_num = get_cell(values, sheet_cls.ACCREDITATION_NUMBER)
    if is_not_empty(acc_num) and not is_valid_accreditation_number(acc_num):
        errors.append(
            f"Accreditation number {acc_num} is not a valid accreditation number."
        )

    acc_date = get_cell(values, sheet_cls.ACCREDITATION_CHANGE_DATE)
    if is_not_empty(acc_date) and not is_valid_year_range(acc_date):
        errors.append(
            f"Date accreditation status changed {acc_date} is not a valid year range."
        )

    gov = get_cell(values, sheet_cls.GOVERNANCE)
    if not _in_allowed_set(gov, GOVERNANCE_VALUES):
        errors.append(f"Governance {gov} is not a valid governance type.")

    prev_gov = get_cell(values, sheet_cls.PREVIOUS_GOVERNANCE)
    if is_not_empty(prev_gov) and not _in_allowed_set(prev_gov, GOVERNANCE_VALUES):
        errors.append(f"Previous governance {prev_gov} is not a valid governance type.")

    prev_start = get_cell(values, sheet_cls.PREVIOUS_GOVERNANCE_START)
    if is_not_empty(prev_start) and not is_valid_year_range(prev_start):
        errors.append(
            f"Start date of previous governance {prev_start} is not a valid year range."
        )

    prev_end = get_cell(values, sheet_cls.PREVIOUS_GOVERNANCE_END)
    if is_not_empty(prev_end) and not is_valid_year_range(prev_end):
        errors.append(
            f"End date of previous governance {prev_end} is not a valid year range."
        )

    size = get_cell(values, sheet_cls.SIZE)
    if not _in_allowed_set(size, SIZE_VALUES):
        errors.append(f"Size {size} is not a valid museum size.")

    subject = get_cell(values, sheet_cls.SUBJECT)
    if not _in_allowed_set(subject, SUBJECT_VALUES):
        errors.append(f"Subject {subject} is not a valid museum subject matter.")

    year_opened = get_cell(values, sheet_cls.YEAR_OPENED)
    if not is_valid_year_range(year_opened):
        errors.append(f"Year opened {year_opened} is not a valid year range.")

    year_closed = get_cell(values, sheet_cls.YEAR_CLOSED)
    if not is_valid_year_range(year_closed):
        errors.append(f"Year closed {year_closed} is not a valid year range.")

    return errors
