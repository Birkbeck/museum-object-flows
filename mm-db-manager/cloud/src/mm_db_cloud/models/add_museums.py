from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, List, Optional


@dataclass(frozen=True)
class AddMuseumsRequest:
    pass


@dataclass(frozen=True)
class RowError:
    row: int  # 1-indexed row number on Add sheet
    errors: List[str]


@dataclass(frozen=True)
class AddMuseumsResponse:
    ok: bool
    addedCount: int
    errorsByRow: List[RowError]
    skippedNotReady: int
    message: str

    def to_dict(self) -> Dict[str, Any]:
        return {
            "ok": self.ok,
            "addedCount": self.addedCount,
            "errorsByRow": [
                {"row": e.row, "errors": e.errors} for e in self.errorsByRow
            ],
            "skippedNotReady": self.skippedNotReady,
            "message": self.message,
        }
