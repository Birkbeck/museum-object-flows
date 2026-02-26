from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, List, Optional


@dataclass(frozen=True)
class RowError:
    row: int
    errors: List[str]


@dataclass(frozen=True)
class PermanentlyDeleteMuseumsRequest:
    pass


@dataclass(frozen=True)
class PermanentlyDeleteMuseumsResponse:
    ok: bool
    deletedCount: int
    errorsByRow: List[RowError]
    skippedNotMarked: int
    message: str

    def to_dict(self) -> Dict[str, Any]:
        return {
            "ok": self.ok,
            "deletedCount": self.deletedCount,
            "errorsByRow": [
                {"row": e.row, "errors": e.errors} for e in self.errorsByRow
            ],
            "skippedNotReady": self.skippedNotMarked,
            "message": self.message,
        }
