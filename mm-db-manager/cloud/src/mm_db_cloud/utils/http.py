from __future__ import annotations

from typing import Any, Dict, Tuple
from flask import jsonify


def ok(data: Dict[str, Any] | None = None, status: int = 200) -> Tuple[Any, int]:
    payload = {"ok": True}
    if data:
        payload.update(data)
    return jsonify(payload), status


def fail(message: str, status: int = 400, details: Dict[str, Any] | None = None) -> Tuple[Any, int]:
    payload: Dict[str, Any] = {"ok": False, "error": message}
    if details:
        payload["details"] = details
    return jsonify(payload), status
