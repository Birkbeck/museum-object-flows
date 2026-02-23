# sheet_to_graph/anonymize_rows.py
from __future__ import annotations

import os
from typing import Dict, List, Tuple


def _env_int(name: str, default: int) -> int:
    """Read integer env var with fallback."""
    value = os.environ.get(name)
    if value is None:
        return default
    try:
        return int(value)
    except ValueError:
        raise ValueError(f"Environment variable {name} must be an integer")


# ---- Column indices (0-based, configurable via env vars) ----
ACTOR_ID_COL = _env_int("ACTOR_ID_COL", 0)
ACTOR_NAME_COL = _env_int("ACTOR_NAME_COL", 1)
ACTOR_TYPE_COL = _env_int("ACTOR_TYPE_COL", 2)
ACTOR_ADDR1_COL = _env_int("ACTOR_ADDR1_COL", 5)
ACTOR_POSTCODE_COL = _env_int("ACTOR_POSTCODE_COL", 8)
ACTOR_NOTES_COL = _env_int("ACTOR_NOTES_COL", 12)

RECIPIENT_ID_COL = _env_int("RECIPIENT_ID_COL", 27)
RECIPIENT_NAME_COL = _env_int("RECIPIENT_NAME_COL", 28)
EVENT_NOTES_COL = _env_int("EVENT_NOTES_COL", 34)

ANONYMIZE_ACTOR_TYPE_VALUE = os.environ.get("ANONYMIZE_ACTOR_TYPE_VALUE", "individual")


def _ensure_len(row: List[str], length: int) -> List[str]:
    if len(row) >= length:
        return row
    return row + [""] * (length - len(row))


def _rectangular(values: List[List[str]]) -> List[List[str]]:
    if not values:
        return values
    width = max(len(r) for r in values)
    return [_ensure_len(list(r), width) for r in values]


def anonymize_actor_and_event_rows(
    actor_rows: List[List[str]],
    event_rows: List[List[str]],
) -> Tuple[List[List[str]], List[List[str]]]:
    """
    In-memory anonymization used inside translate pipeline.

    Controlled entirely by environment variables for column indices.

    Behaviour:
    - Clear actor notes for all actors.
    - For actors where actor_type == ANONYMIZE_ACTOR_TYPE_VALUE:
        * actor_id -> p{n}
        * actor_name -> person{n}
        * clear address1 and postcode
        * store mapping old_id -> (new_id, new_name)
    - Clear event notes.
    - Replace recipient_id + recipient_name if recipient_id was anonymized.
    """

    actor_rows = _rectangular(actor_rows)
    event_rows = _rectangular(event_rows)

    if not actor_rows:
        return actor_rows, event_rows

    a_header = actor_rows[0]
    a_data = actor_rows[1:] if len(actor_rows) > 1 else []

    id_map: Dict[str, Tuple[str, str]] = {}
    counter = 0

    # ---- Anonymize actors ----
    for row in a_data:
        row = _ensure_len(row, len(a_header))

        # Clear notes
        if ACTOR_NOTES_COL < len(row):
            row[ACTOR_NOTES_COL] = ""

        actor_type = row[ACTOR_TYPE_COL] if ACTOR_TYPE_COL < len(row) else ""

        if actor_type == ANONYMIZE_ACTOR_TYPE_VALUE:
            old_id = str(row[ACTOR_ID_COL]) if ACTOR_ID_COL < len(row) else ""

            new_id = f"p{counter}"
            new_name = f"person{counter}"
            counter += 1

            if old_id:
                id_map[old_id] = (new_id, new_name)

            if ACTOR_ID_COL < len(row):
                row[ACTOR_ID_COL] = new_id
            if ACTOR_NAME_COL < len(row):
                row[ACTOR_NAME_COL] = new_name
            if ACTOR_ADDR1_COL < len(row):
                row[ACTOR_ADDR1_COL] = ""
            if ACTOR_POSTCODE_COL < len(row):
                row[ACTOR_POSTCODE_COL] = ""

    actors_out = _rectangular([a_header] + a_data)

    # ---- Anonymize events ----
    if not event_rows:
        return actors_out, event_rows

    e_header = event_rows[0]
    e_data = event_rows[1:] if len(event_rows) > 1 else []

    for row in e_data:
        row = _ensure_len(row, len(e_header))

        # Clear event notes
        if EVENT_NOTES_COL < len(row):
            row[EVENT_NOTES_COL] = ""

        old_rid = str(row[RECIPIENT_ID_COL]) if RECIPIENT_ID_COL < len(row) else ""

        if old_rid and old_rid in id_map:
            new_id, new_name = id_map[old_rid]

            if RECIPIENT_ID_COL < len(row):
                row[RECIPIENT_ID_COL] = new_id
            if RECIPIENT_NAME_COL < len(row):
                row[RECIPIENT_NAME_COL] = new_name

    events_out = _rectangular([e_header] + e_data)

    return actors_out, events_out
