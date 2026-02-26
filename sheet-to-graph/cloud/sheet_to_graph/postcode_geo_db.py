import os
import re
import sqlite3
from typing import Any, Dict, Optional

from bng_latlon import WGS84toOSGB36

from .wikidata_connection import WikidataConnection


def _blank_geo() -> Dict[str, Any]:
    return {
        "latitude": None,
        "longitude": None,
        "bng_x": None,
        "bng_y": None,
        "region": "",
        "country": "",
        "lad_code": "",
        "lad": "",
    }


class PostcodeGeoDB:
    """
    Resolver backed by SQLite with cache-aside behaviour.
    Includes:
      - UK postcode lookups (preloaded in DB)
      - Wikidata-derived lookups for:
          * city+country (non-UK)
          * town+county (UK no postcode / fallback)

    Tables expected:

    -- UK postcodes (preloaded)
    CREATE TABLE IF NOT EXISTS postcode_lookup (
      postcode   TEXT PRIMARY KEY,
      latitude   REAL,
      longitude  REAL,
      bng_x      REAL,
      bng_y      REAL,
      region     TEXT,
      country    TEXT,
      lad_code   TEXT,
      lad        TEXT
    );

    -- Cache: "Town, County" -> geo info
    CREATE TABLE IF NOT EXISTS town_county_lookup (
      key        TEXT PRIMARY KEY,
      latitude   REAL,
      longitude  REAL,
      bng_x      REAL,
      bng_y      REAL,
      region     TEXT,
      country    TEXT,
      lad_code   TEXT,
      lad        TEXT
    );

    -- Cache: "City, Country" (non-UK) -> geo info
    CREATE TABLE IF NOT EXISTS city_country_lookup (
      key        TEXT PRIMARY KEY,
      latitude   REAL,
      longitude  REAL,
      bng_x      REAL,
      bng_y      REAL,
      region     TEXT,
      country    TEXT,
      lad_code   TEXT,
      lad        TEXT
    );
    """

    def __init__(
        self,
        sqlite_path: str,
        wikidata_connection: WikidataConnection,
        *,
        timeout_seconds: float = 30.0,
    ):
        self.sqlite_path = sqlite_path
        self.wikidata_connection = wikidata_connection
        self.timeout_seconds = timeout_seconds
        self._conn: Optional[sqlite3.Connection] = None

    # ---- context manager ----
    def __enter__(self) -> "PostcodeGeoDB":
        self._conn = sqlite3.connect(self.sqlite_path, timeout=self.timeout_seconds)
        self._conn.row_factory = sqlite3.Row
        self._ensure_schema()
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        if self._conn is not None:
            try:
                if exc_type is None:
                    self._conn.commit()
            finally:
                try:
                    self._conn.close()
                finally:
                    self._conn = None
        return False

    # ---- public API (dict output) ----
    def get_geo_info(
        self,
        postcode: str,
        town_city: str = "",
        county: str = "",
        country: str = "",
    ) -> Dict[str, Any]:
        """
        Main resolver used by the PostcodeToLatLong compatibility wrapper below.
        """
        # Non-UK: cache by "town_city, country" (or just country)
        if country and country not in (
            "England",
            "Scotland",
            "Wales",
            "Northern Ireland",
            "UK",
            "United Kingdom",
        ):
            key = (
                f"{town_city}, {country}".strip(", ").strip() if town_city else country
            )
            return self._get_or_create_city_country(key)

        if postcode:
            # UK postcode lookup table should already contain full details
            row = self._fetch_postcode(postcode)
            if row is not None:
                return self._row_to_geo(row)
            # If postcode missing from DB, return blank (we are explicitly not scanning CSVs anymore)
            return _blank_geo()

        # UK with no postcode: cache by "town, county" etc.
        if town_city or county:
            if town_city and county:
                key = f"{town_city}, {county}"
            elif town_city:
                key = town_city
            else:
                key = county
            return self._get_or_create_town_county(key)

        return _blank_geo()

    # ---- internal: schema + helpers ----
    def _ensure_conn(self) -> sqlite3.Connection:
        if self._conn is None:
            self._conn = sqlite3.connect(self.sqlite_path, timeout=self.timeout_seconds)
            self._conn.row_factory = sqlite3.Row
            self._ensure_schema()
        return self._conn

    def _ensure_schema(self) -> None:
        conn = self._ensure_conn()
        conn.execute(
            """
            CREATE TABLE IF NOT EXISTS postcode_lookup (
              postcode   TEXT PRIMARY KEY,
              latitude   REAL,
              longitude  REAL,
              bng_x      REAL,
              bng_y      REAL,
              region     TEXT,
              country    TEXT,
              lad_code   TEXT,
              lad        TEXT
            )
            """
        )
        conn.execute(
            """
            CREATE TABLE IF NOT EXISTS town_county_lookup (
              key        TEXT PRIMARY KEY,
              latitude   REAL,
              longitude  REAL,
              bng_x      REAL,
              bng_y      REAL,
              region     TEXT,
              country    TEXT,
              lad_code   TEXT,
              lad        TEXT
            )
            """
        )
        conn.execute(
            """
            CREATE TABLE IF NOT EXISTS city_country_lookup (
              key        TEXT PRIMARY KEY,
              latitude   REAL,
              longitude  REAL,
              bng_x      REAL,
              bng_y      REAL,
              region     TEXT,
              country    TEXT,
              lad_code   TEXT,
              lad        TEXT
            )
            """
        )

    def _row_to_geo(self, row: sqlite3.Row) -> Dict[str, Any]:
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

    def _fetch_postcode(self, postcode: str) -> Optional[sqlite3.Row]:
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

    def _fetch_lookup(self, table: str, key: str) -> Optional[sqlite3.Row]:
        conn = self._ensure_conn()
        cur = conn.execute(
            f"""
            SELECT latitude, longitude, bng_x, bng_y, region, country, lad_code, lad
            FROM {table}
            WHERE key = ?
            LIMIT 1
            """,
            (key,),
        )
        return cur.fetchone()

    def _upsert_lookup(self, table: str, key: str, geo: Dict[str, Any]) -> None:
        conn = self._ensure_conn()
        conn.execute(
            f"""
            INSERT INTO {table} (key, latitude, longitude, bng_x, bng_y, region, country, lad_code, lad)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(key) DO UPDATE SET
              latitude=excluded.latitude,
              longitude=excluded.longitude,
              bng_x=excluded.bng_x,
              bng_y=excluded.bng_y,
              region=excluded.region,
              country=excluded.country,
              lad_code=excluded.lad_code,
              lad=excluded.lad
            """,
            (
                key,
                geo.get("latitude"),
                geo.get("longitude"),
                geo.get("bng_x"),
                geo.get("bng_y"),
                geo.get("region", ""),
                geo.get("country", ""),
                geo.get("lad_code", ""),
                geo.get("lad", ""),
            ),
        )

    # ---- non-UK: city_country ----
    def _get_or_create_city_country(self, key: str) -> Dict[str, Any]:
        key = (key or "").strip()
        if not key:
            return _blank_geo()

        cached = self._fetch_lookup("city_country_lookup", key)
        if cached is not None:
            return self._row_to_geo(cached)

        geo = _blank_geo()
        # Special-cases you had before
        if "Channel Islands" in key:
            geo["region"] = "Channel Islands"
            geo["lad"] = "Channel Islands"
        elif "Isle of Man" in key:
            geo["region"] = "Isle of Man"
            geo["lad"] = "Isle of Man"

        try:
            results = self.wikidata_connection.search_entities(key) or []
        except Exception:
            results = []

        for result in results:
            try:
                props = self.wikidata_connection.get_entity_properties(result["id"])
                coords = props["P625"]
                lat = coords["latitude"]
                lon = coords["longitude"]
                bng = WGS84toOSGB36(lat, lon)
                geo.update(
                    {
                        "latitude": lat,
                        "longitude": lon,
                        "bng_x": bng[0],
                        "bng_y": bng[1],
                    }
                )
                break
            except Exception:
                continue

        # Cache it
        self._upsert_lookup("city_country_lookup", key, geo)
        return geo

    # ---- UK no-postcode: town_county ----
    def _get_or_create_town_county(self, key: str) -> Dict[str, Any]:
        key = (key or "").strip()
        if not key:
            return _blank_geo()

        cached = self._fetch_lookup("town_county_lookup", key)
        if cached is not None:
            return self._row_to_geo(cached)

        geo = _blank_geo()
        geo["country"] = "UK"

        # If you have LAD/region inference available elsewhere, you can plug it in here.
        # For now: only Wikidata coordinates (optionally) cached.
        try:
            results = self.wikidata_connection.search_entities(key) or []
        except Exception:
            results = []

        for result in results:
            try:
                props = self.wikidata_connection.get_entity_properties(result["id"])
                # Optional population guard (like your old logic)
                pop_ok = True
                if "P1082" in props:
                    try:
                        population = int(props["P1082"]["amount"][1:]) * int(
                            props["P1082"]["unit"]
                        )
                        pop_ok = population < 1e5
                    except Exception:
                        pop_ok = True

                if pop_ok and "P625" in props:
                    coords = props["P625"]
                    lat = coords["latitude"]
                    lon = coords["longitude"]
                    bng = WGS84toOSGB36(lat, lon)
                    geo.update(
                        {
                            "latitude": lat,
                            "longitude": lon,
                            "bng_x": bng[0],
                            "bng_y": bng[1],
                        }
                    )
                break
            except Exception:
                continue

        self._upsert_lookup("town_county_lookup", key, geo)
        return geo


class PostcodeToLatLong:
    """
    Compatibility wrapper preserving your existing API:
      - get_latitude(postcode, town_city, county, country)
      - get_longitude(...)
      - get_bng_x / get_bng_y / get_region / get_local_authority_code / get_local_authority_name

    Uses a PostcodeGeoDB under the hood.
    """

    def __init__(
        self,
        sqlite_path: str,
        wikidata_connection: WikidataConnection,
        *,
        timeout_seconds: float = 30.0,
    ):
        self._db = PostcodeGeoDB(
            sqlite_path,
            wikidata_connection,
            timeout_seconds=timeout_seconds,
        )

    def __enter__(self) -> "PostcodeToLatLong":
        self._db.__enter__()
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        return self._db.__exit__(exc_type, exc, tb)

    def _info(
        self, postcode: str, town_city: str, county: str, country: str
    ) -> Dict[str, Any]:
        return self._db.get_geo_info(
            postcode, town_city=town_city, county=county, country=country
        )

    def get_latitude(self, postcode: str, town_city: str, county: str, country: str):
        return self._info(postcode, town_city, county, country)["latitude"]

    def get_longitude(self, postcode: str, town_city: str, county: str, country: str):
        return self._info(postcode, town_city, county, country)["longitude"]

    def get_bng_x(self, postcode: str, town_city: str, county: str, country: str):
        return self._info(postcode, town_city, county, country)["bng_x"]

    def get_bng_y(self, postcode: str, town_city: str, county: str, country: str):
        return self._info(postcode, town_city, county, country)["bng_y"]

    def get_region(self, postcode: str, town_city: str, county: str, country: str):
        # Keep your special-cases
        if len(postcode) >= 2:
            if postcode[:2] == "IM":
                return "Isle of Man"
            if postcode[:2] in ("GY", "JE"):
                return "Channel Islands"
        return self._info(postcode, town_city, county, country)["region"]

    def get_local_authority_code(
        self, postcode: str, town_city: str, county: str, country: str
    ):
        return self._info(postcode, town_city, county, country)["lad_code"]

    def get_local_authority_name(
        self, postcode: str, town_city: str, county: str, country: str
    ):
        return self._info(postcode, town_city, county, country)["lad"]
