from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, List, Optional


@dataclass(frozen=True)
class RowError:
    row: int
    errors: List[str]


@dataclass(frozen=True)
class TrashMuseumsRequest:
    pass


@dataclass(frozen=True)
class TrashMuseumsResponse:
    ok: bool
    trashedCount: int
    errorsByRow: List[RowError]
    skippedNotReady: int
    message: str

    def to_dict(self) -> Dict[str, Any]:
        return {
            "ok": self.ok,
            "trashedCount": self.trashedCount,
            "errorsByRow": [
                {"row": e.row, "errors": e.errors} for e in self.errorsByRow
            ],
            "skippedNotReady": self.skippedNotReady,
            "message": self.message,
        }
