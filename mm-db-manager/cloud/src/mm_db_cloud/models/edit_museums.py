from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, List


@dataclass(frozen=True)
class EditMuseumsRequest:
    pass


@dataclass(frozen=True)
class RowError:
    row: int
    errors: List[str]


@dataclass(frozen=True)
class EditMuseumsResponse:
    ok: bool
    editedCount: int
    errorsByRow: List[RowError]
    skippedNotReady: int
    message: str

    def to_dict(self) -> Dict[str, Any]:
        return {
            "ok": self.ok,
            "editedCount": self.editedCount,
            "errorsByRow": [
                {"row": e.row, "errors": e.errors} for e in self.errorsByRow
            ],
            "skippedNotReady": self.skippedNotReady,
            "message": self.message,
        }
