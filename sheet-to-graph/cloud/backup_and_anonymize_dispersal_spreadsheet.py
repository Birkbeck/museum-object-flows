from datetime import datetime
import json
from typing import Dict, Tuple, List
import zoneinfo

from sheet_to_graph import GoogleUtils

ACTORS_SHEET = "model-v1-actors"
EVENTS_SHEET = "model-v1-events"
ACTOR_TYPES_SHEET = "actor-types-hierarchy"
EVENT_TYPES_SHEET = "event-types-hierarchy"
SUPER_EVENT_TYPES_SHEET = "super-event-types-hierarchy"
DEFAULT_RECIPIENT_TYPES_SHEET = "default-recipient-types"
CLOSURE_CAUSES_HIERARCHY = "closure-causes-hierarchy"

ACTOR_ID_COL = 0
ACTOR_NAME_COL = 1
ACTOR_TYPE_COL = 2
ACTOR_ADDR1_COL = 5
ACTOR_POSTCODE_COL = 8
ACTOR_NOTES_COL = 12
RECIPIENT_ID_COL = 27
RECIPIENT_NAME_COL = 28
EVENT_NOTES_COL = 34


def backup_spreadsheet(spreadsheet_id: str, backup_directory_id: str) -> dict:
    """
    Makes a copy of the spreadsheet (Drive file) and appends a timestamp to the new file's name.
    Returns the new file's minimal metadata (id, name, webViewLink).
    """
    drive = GoogleUtils.get_drive_service()
    source_file_meta_data = (
        drive.files().get(fileId=spreadsheet_id, fields="name, parents").execute()
    )
    british_time = datetime.now(zoneinfo.ZoneInfo("Europe/London")).strftime(
        "%Y%m%d_%H%M%S"
    )
    destination_file_meta_data = {
        "name": f"{source_file_meta_data['name']}—backup-{british_time}",
        "parents": [backup_directory_id],
    }
    new_file = (
        drive.files()
        .copy(
            fileId=spreadsheet_id,
            body=destination_file_meta_data,
            fields="id, name, webViewLink",
        )
        .execute()
    )
    return new_file


def _ensure_len(row: List, length: int) -> List:
    """Pad row to the given length."""
    return row + [""] * (length - len(row))


def _rectangular(values: List[List]) -> List[List]:
    """Pad all rows to the same max length."""
    if not values:
        return values
    width = max(len(r) for r in values)
    return [_ensure_len(list(r), width) for r in values]


def anonymize_spreadsheet(spreadsheet_id: str, output_spreadsheet_id: str) -> None:
    """
    Read SOURCE spreadsheet once, anonymize in memory, then:
      1) clear both tabs in OUTPUT
      2) write full anonymized content in one batchUpdate.
    """
    sheets = GoogleUtils.get_sheets_service()

    origin_sheets = (
        sheets.spreadsheets()
        .values()
        .batchGet(
            spreadsheetId=spreadsheet_id,
            ranges=[
                ACTORS_SHEET,
                EVENTS_SHEET,
                ACTOR_TYPES_SHEET,
                EVENT_TYPES_SHEET,
                SUPER_EVENT_TYPES_SHEET,
                DEFAULT_RECIPIENT_TYPES_SHEET,
                CLOSURE_CAUSES_HIERARCHY,
            ],
            majorDimension="ROWS",
        )
        .execute()
    )
    ranges = origin_sheets.get("valueRanges", [])
    by_name = {
        vr["range"].split("!")[0].strip("'"): vr.get("values", []) for vr in ranges
    }

    actors_values = _rectangular(by_name.get(ACTORS_SHEET, []))
    events_values = _rectangular(by_name.get(EVENTS_SHEET, []))
    actor_types_values = _rectangular(by_name.get(ACTOR_TYPES_SHEET, []))
    event_types_values = _rectangular(by_name.get(EVENT_TYPES_SHEET, []))
    super_event_types_values = _rectangular(by_name.get(SUPER_EVENT_TYPES_SHEET, []))
    default_recipient_types_values = _rectangular(
        by_name.get(DEFAULT_RECIPIENT_TYPES_SHEET, [])
    )
    closure_causes_hierarchy_values = _rectangular(
        by_name.get(CLOSURE_CAUSES_HIERARCHY, [])
    )

    a_header = actors_values[0]
    a_rows = actors_values[1:] if len(actors_values) > 1 else []

    id_map: Dict[str, Tuple[str, str]] = {}
    counter = 0
    for i, row in enumerate(a_rows):
        a_rows[i] = row
        row[ACTOR_NOTES_COL] = ""
        if row[ACTOR_TYPE_COL] == "individual":
            old_id = str(row[ACTOR_ID_COL]) if row[ACTOR_ID_COL] != "" else ""
            new_id = f"p{counter}"
            new_name = f"person{counter}"
            counter += 1
            if old_id:
                id_map[old_id] = (new_id, new_name)
            row[ACTOR_ID_COL] = new_id
            row[ACTOR_NAME_COL] = new_name
            row[ACTOR_ADDR1_COL] = ""
            row[ACTOR_POSTCODE_COL] = ""

    actors_out = [a_header] + a_rows

    if events_values:
        e_header = events_values[0]
        e_rows = events_values[1:] if len(events_values) > 1 else []
        for i, row in enumerate(e_rows):
            e_rows[i] = row
            row[EVENT_NOTES_COL] = ""
            old_rid = str(row[RECIPIENT_ID_COL]) if row[RECIPIENT_ID_COL] != "" else ""
            if old_rid in id_map:
                new_id, new_name = id_map[old_rid]
                row[RECIPIENT_ID_COL] = new_id
                row[RECIPIENT_NAME_COL] = new_name

        events_out = [e_header] + e_rows
    else:
        events_out = []

    actors_out = _rectangular(actors_out)
    events_out = _rectangular(events_out)

    sheets.spreadsheets().values().batchClear(
        spreadsheetId=output_spreadsheet_id,
        body={
            "ranges": [
                ACTORS_SHEET,
                EVENTS_SHEET,
                ACTOR_TYPES_SHEET,
                EVENT_TYPES_SHEET,
                SUPER_EVENT_TYPES_SHEET,
                DEFAULT_RECIPIENT_TYPES_SHEET,
                CLOSURE_CAUSES_HIERARCHY,
            ]
        },
    ).execute()

    data = [
        {
            "range": ACTORS_SHEET,
            "majorDimension": "ROWS",
            "values": actors_out,
        },
        {
            "range": EVENTS_SHEET,
            "majorDimension": "ROWS",
            "values": events_out,
        },
        {
            "range": ACTOR_TYPES_SHEET,
            "majorDimension": "ROWS",
            "values": actor_types_values,
        },
        {
            "range": EVENT_TYPES_SHEET,
            "majorDimension": "ROWS",
            "values": event_types_values,
        },
        {
            "range": SUPER_EVENT_TYPES_SHEET,
            "majorDimension": "ROWS",
            "values": super_event_types_values,
        },
        {
            "range": DEFAULT_RECIPIENT_TYPES_SHEET,
            "majorDimension": "ROWS",
            "values": default_recipient_types_values,
        },
        {
            "range": CLOSURE_CAUSES_HIERARCHY,
            "majorDimension": "ROWS",
            "values": closure_causes_hierarchy_values,
        },
    ]

    sheets.spreadsheets().values().batchUpdate(
        spreadsheetId=output_spreadsheet_id,
        body={"valueInputOption": "RAW", "data": data},
    ).execute()


if __name__ == "__main__":
    with open("config.json") as f:
        config = json.load(f)
    spreadsheet_id = config["dispersal_sheet_input_id"]
    backup_directory_id = config["dispersal_sheet_backups_id"]
    anonymous_spreadsheet_id = config["dispersal_sheet_anon_id"]
    backup_info = backup_spreadsheet(spreadsheet_id, backup_directory_id)
    print("Backup created:")
    print(f"ID: {backup_info['id']}")
    print(f"Name: {backup_info['name']}")
    print(f"Link: {backup_info['webViewLink']}")
    anonymize_spreadsheet(spreadsheet_id, anonymous_spreadsheet_id)
