import csv
import os
import sqlite3
from typing import Dict

import pandas as pd
from bng_latlon import WGS84toOSGB36

ONS_PCD_DIR = "../data/ONSPD_FEB_2024_UK/Data/multi_csv"
LADS_MAP_FILE = (
    "../data/ONSPD_FEB_2024_UK/Documents/LAD23_LAU121_ITL321_ITL221_ITL121_UK_LU.csv"
)
CROWN_DEPS_FILE = "crown_dependency_postcode_lookup.csv"
SQLITE_OUT = "postcode_lookup.sqlite"

REGIONS_MAP: Dict[str, str] = {
    "E12000001": "North East",
    "E12000002": "North West",
    "E12000003": "Yorks & Humber",
    "E12000004": "East Midlands",
    "E12000005": "West Midlands",
    "E12000006": "East of England",
    "E12000007": "London",
    "E12000008": "South East",
    "E12000009": "South West",
    "S99999999": "Scotland",
    "W99999999": "Wales",
    "N99999999": "Northern Ireland",
    "L99999999": "Channel Islands",
    "M99999999": "Isle of Man",
}
COUNTRIES_MAP: Dict[str, str] = {
    "E12000001": "England",
    "E12000002": "England",
    "E12000003": "England",
    "E12000004": "England",
    "E12000005": "England",
    "E12000006": "England",
    "E12000007": "England",
    "E12000008": "England",
    "E12000009": "England",
    "S99999999": "Scotland",
    "W99999999": "Wales",
    "N99999999": "Northern Ireland",
    "L99999999": "Channel Islands",
    "M99999999": "Isle of Man",
}


def load_lads_map(path: str) -> Dict[str, str]:
    with open(path, "r") as f:
        lad_table = csv.DictReader(f)
        return {row["\ufeffLAD23CD"]: row["LAD23NM"] for row in lad_table}


def normalize_postcodes(df: pd.DataFrame) -> pd.DataFrame:
    """
    Take a dataframe with columns pcd, pcd2, pcds + geo columns,
    return a dataframe with a single column 'postcode' and duplicates removed.
    """
    for col in ("pcd", "pcd2", "pcds"):
        if col not in df.columns:
            df[col] = None
    base_cols = [
        "latitude",
        "longitude",
        "bng_x",
        "bng_y",
        "rgn",
        "region",
        "country",
        "lad_code",
        "lad",
    ]
    frames = []
    for col in ("pcd", "pcd2", "pcds"):
        tmp = df[[col] + base_cols].rename(columns={col: "postcode"})
        frames.append(tmp)
    out = pd.concat(frames, ignore_index=True)
    out["postcode"] = out["postcode"].replace({"": pd.NA, "nan": pd.NA, "none": pd.NA})
    out = out.dropna(subset=["postcode"])
    out = out.drop_duplicates(subset=["postcode"])
    return out


def ensure_schema(conn: sqlite3.Connection) -> None:
    conn.execute(
        """
        CREATE TABLE IF NOT EXISTS postcode_lookup (
            postcode   TEXT PRIMARY KEY,
            latitude   REAL,
            longitude  REAL,
            bng_x      REAL,
            bng_y      REAL,
            rgn        TEXT,
            region     TEXT,
            country    TEXT,
            lad_code   TEXT,
            lad        TEXT
        );
        """
    )
    conn.execute(
        "CREATE INDEX IF NOT EXISTS idx_postcode_lad_code ON postcode_lookup(lad_code);"
    )
    conn.execute(
        "CREATE INDEX IF NOT EXISTS idx_postcode_region ON postcode_lookup(region);"
    )


def insert_batch(conn: sqlite3.Connection, batch: pd.DataFrame) -> None:
    """
    Insert with upsert so reruns don't fail. If a postcode already exists,
    keep the existing row (or update—choose your policy).
    """
    rows = list(
        batch[
            [
                "postcode",
                "latitude",
                "longitude",
                "bng_x",
                "bng_y",
                "rgn",
                "region",
                "country",
                "lad_code",
                "lad",
            ]
        ].itertuples(index=False, name=None)
    )
    conn.executemany(
        """
        INSERT INTO postcode_lookup (
            postcode, latitude, longitude, bng_x, bng_y, rgn, region, country, lad_code, lad
        )
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(postcode) DO UPDATE SET
            latitude=excluded.latitude,
            longitude=excluded.longitude,
            bng_x=excluded.bng_x,
            bng_y=excluded.bng_y,
            rgn=excluded.rgn,
            region=excluded.region,
            country=excluded.country,
            lad_code=excluded.lad_code,
            lad=excluded.lad
        ;
        """,
        rows,
    )


if __name__ == "__main__":
    lads_map = load_lads_map(LADS_MAP_FILE)
    crown_dependencies = pd.read_csv(CROWN_DEPS_FILE)
    crown_dependencies_lat_map = {
        row["postcode"]: row["latitude"]
        for row in crown_dependencies.to_dict(orient="records")
    }
    crown_dependencies_lon_map = {
        row["postcode"]: row["longitude"]
        for row in crown_dependencies.to_dict(orient="records")
    }

    conn = sqlite3.connect(SQLITE_OUT)
    try:
        ensure_schema(conn)
        conn.execute("PRAGMA journal_mode=WAL;")
        conn.execute("PRAGMA synchronous=NORMAL;")
        for file_name in sorted(os.listdir(ONS_PCD_DIR)):
            if not file_name.lower().endswith(".csv"):
                continue
            if file_name.lower().startswith("~$"):
                continue
            postcode_file = os.path.join(ONS_PCD_DIR, file_name)
            print(f"Processing {postcode_file} ...")
            df = pd.read_csv(postcode_file, dtype=str)
            df["lat"] = pd.to_numeric(df["lat"], errors="coerce")
            df["long"] = pd.to_numeric(df["long"], errors="coerce")
            df = df.rename(
                columns={
                    "lat": "latitude",
                    "long": "longitude",
                    "oslaua": "lad_code",
                }
            )
            keep = ["pcd", "pcd2", "pcds", "latitude", "longitude", "rgn", "lad_code"]
            df = df[keep]
            df["latitude"] = df.apply(
                lambda row: row["latitude"]
                if row["pcd"] not in crown_dependencies_lat_map
                else crown_dependencies_lat_map[row["pcd"]],
                axis=1,
            )
            df["longitude"] = df.apply(
                lambda row: row["longitude"]
                if row["pcd"] not in crown_dependencies_lon_map
                else crown_dependencies_lon_map[row["pcd"]],
                axis=1,
            )
            df["lad"] = df["lad_code"].map(lads_map)
            df["region"] = df["rgn"].map(REGIONS_MAP)
            df["country"] = df["rgn"].map(COUNTRIES_MAP)
            bng = df.apply(
                lambda r: WGS84toOSGB36(r["latitude"], r["longitude"]), axis=1
            )
            df["bng_x"] = [
                xy[0] if isinstance(xy, (list, tuple)) else None for xy in bng
            ]
            df["bng_y"] = [
                xy[1] if isinstance(xy, (list, tuple)) else None for xy in bng
            ]
            batch = normalize_postcodes(df)
            with conn:
                insert_batch(conn, batch)
        print("Done.")
    finally:
        conn.close()
