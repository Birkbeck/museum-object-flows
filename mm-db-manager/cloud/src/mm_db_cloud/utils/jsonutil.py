from __future__ import annotations

from typing import Any, Dict
from flask import Request


def get_json_or_raise(request: Request) -> Dict[str, Any]:
    payload = request.get_json(force=True, silent=False)
    if not isinstance(payload, dict):
        raise ValueError("Request JSON must be an object")
    return payload
