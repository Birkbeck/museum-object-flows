import os
import sqlite3
from dataclasses import dataclass
from typing import Any, Dict, Optional


@dataclass(frozen=True)
class GeoInfo:
    latitude: Optional[float]
    longitude: Optional[float]
    bng_x: Optional[float]
    bng_y: Optional[float]
    region: str
    country: str
    lad_code: str
    lad: str


class PostcodeGeoDB:
    """
    Lightweight postcode -> geo-info resolver backed by a SQLite database.
    SQLite DB contains a table `postcode_lookup` with (at least):
      postcode TEXT PRIMARY KEY,
      latitude REAL,
      longitude REAL,
      bng_x REAL,
      bng_y REAL,
      region TEXT,
      country TEXT,
      lad_code TEXT,
      lad TEXT
    """

    def __init__(self, sqlite_path: str, *, timeout_seconds: float = 30.0):
        self.sqlite_path = sqlite_path
        self.timeout_seconds = timeout_seconds
        self._conn: Optional[sqlite3.Connection] = None

    # ---- Context manager so callers can `with PostcodeGeoDB(...) as db:` ----
    def __enter__(self) -> "PostcodeGeoDB":
        self._conn = sqlite3.connect(self.sqlite_path, timeout=self.timeout_seconds)
        # rows as dict-like
        self._conn.row_factory = sqlite3.Row
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        if self._conn is not None:
            try:
                self._conn.close()
            finally:
                self._conn = None
        return False  # don't suppress exceptions

    # ---- Public API ----
    def get_geo_info(self, postcode: str) -> Dict[str, Any]:
        """
        Return geo info dict for a postcode. If not found (or postcode blank), returns
        a blank/empty structure with None coords and empty strings for text fields.

        Important: this does NOT normalize the postcode. If your input postcodes may vary
        (e.g. 'sw1a1aa' vs 'SW1A 1AA'), normalize *before* calling.
        """
        blank = {
            "latitude": None,
            "longitude": None,
            "bng_x": None,
            "bng_y": None,
            "region": "",
            "country": "",
            "lad_code": "",
            "lad": "",
        }
        if not postcode:
            return blank

        row = self._fetch_row(postcode)
        if row is None:
            return blank

        return {
            "latitude": float(row["latitude"]) if row["latitude"] is not None else None,
            "longitude": float(row["longitude"])
            if row["longitude"] is not None
            else None,
            "bng_x": float(row["bng_x"]) if row["bng_x"] is not None else None,
            "bng_y": float(row["bng_y"]) if row["bng_y"] is not None else None,
            "region": row["region"] or "",
            "country": row["country"] or "",
            "lad_code": row["lad_code"] or "",
            "lad": row["lad"] or "",
        }

    # ---- Internals ----
    def _fetch_row(self, postcode: str) -> Optional[sqlite3.Row]:
        conn = self._ensure_conn()
        cur = conn.execute(
            """
            SELECT latitude, longitude, bng_x, bng_y, region, country, lad_code, lad
            FROM postcode_lookup
            WHERE postcode = ?
            LIMIT 1
            """,
            (postcode,),
        )
        return cur.fetchone()

    def _ensure_conn(self) -> sqlite3.Connection:
        if self._conn is None:
            # Allow non-context usage, but context manager is preferred.
            self._conn = sqlite3.connect(self.sqlite_path, timeout=self.timeout_seconds)
            self._conn.row_factory = sqlite3.Row
        return self._conn
