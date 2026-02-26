from __future__ import annotations

from typing import Any, Optional


def as_trimmed_string(value: Any) -> str:
    if value is None:
        return ""
    return str(value).strip()


def parse_museum_id(museum_cell: Any) -> Optional[str]:
    """
    Expects "id - name" (exactly what your TS expects).
    Returns extracted id or None.
    """
    s = as_trimmed_string(museum_cell)
    if not s:
        return None
    # Split on first " - "
    if " - " not in s:
        return None
    museum_id, _name = s.split(" - ", 1)
    museum_id = museum_id.strip()
    return museum_id or None
