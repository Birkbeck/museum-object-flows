from __future__ import annotations

import os


def get_pinned_spreadsheet_id() -> str:
    pinned = os.environ.get("MM_DB_SPREADSHEET_ID")
    if not pinned:
        raise ValueError("MM_DB_SPREADSHEET_ID is not set on the server.")
    return pinned
