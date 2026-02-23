"""
This script loads data from human readable Google Sheets
and saves the data in machine readable csv files on Google Drive.
The script does the following:
- it specifies a schema for a set of spreadsheets (or csv files);
- it reads files containing the sheets and validates them;
- it infers extra fields in the tables;
- it generates csv representations.
"""
import hmac
import json
import logging
import os
import re
import time
from typing import Any, Dict, Tuple

from google.cloud import storage

from sheet_to_graph import (
    FileLoader,
    GoogleUtils,
    PostcodeToLatLong,
    WikidataConnection,
)
from sheet_to_graph.validate_and_translate import validate_and_translate_data
from sheet_to_graph.cloud_storage_utils import _gcs_bucket, _upload_df_csv


def resolve_postcode_sqlite_path() -> str:
    local_path = os.environ.get("POSTCODE_GEO_DB", "/tmp/postcode_lookup.sqlite")
    if os.path.exists(local_path):
        return local_path

    gcs_uri = os.environ.get("POSTCODE_GEO_DB_GCS_URI")
    if not gcs_uri:
        raise RuntimeError(
            f"SQLite DB not found at {local_path} and POSTCODE_GEO_DB_GCS_URI not set."
        )
    if not gcs_uri.startswith("gs://"):
        raise ValueError("POSTCODE_GEO_DB_GCS_URI must start with gs://")

    bucket_name, blob_name = gcs_uri[5:].split("/", 1)
    client = storage.Client()
    bucket = client.bucket(bucket_name)
    blob = bucket.blob(blob_name)
    blob.download_to_filename(local_path)
    return local_path


def upload_sqlite_back(local_path: str, gcs_uri: str) -> None:
    if not gcs_uri.startswith("gs://"):
        raise ValueError("POSTCODE_GEO_DB_GCS_URI must start with gs://")
    bucket_name, blob_name = gcs_uri[5:].split("/", 1)
    client = storage.Client()
    bucket = client.bucket(bucket_name)
    blob = bucket.blob(blob_name)
    blob.upload_from_filename(local_path, content_type="application/x-sqlite3")


def run_translate() -> Tuple[int, Dict[str, Any]]:
    start = time.time()

    file_loader = FileLoader(GoogleUtils.get_sheets_service)

    sqlite_path = resolve_postcode_sqlite_path()
    gcs_uri = os.environ.get("POSTCODE_GEO_DB_GCS_URI", "")

    with PostcodeToLatLong(
        sqlite_path,
        WikidataConnection(os.environ["EMAIL"]),
    ) as p:
        urls, timestamp = validate_and_translate_data(file_loader, p)

    # Persist cached Wikidata results back to GCS (optional but recommended)
    uploaded_geo_db = False
    if gcs_uri:
        upload_sqlite_back(sqlite_path, gcs_uri)
        uploaded_geo_db = True

    elapsed = time.time() - start

    meta = {
        "status": "success",
        "seconds": elapsed,
        "timestamp": timestamp,
        "bucket": os.environ.get("SNAPSHOT_BUCKET", ""),
        "urls": urls,
        "postcode_geo_db_gcs_uri": gcs_uri,
        "postcode_geo_db_uploaded": uploaded_geo_db,
    }
    return 0, meta


def translate(request):
    """
    Cloud Function entrypoint (HTTP), publisher-style:
    - Auth via X-Translate-Token header matching TRANSLATE_TOKEN env var.
    - POST only.
    - Returns (dict, status_code).
    """
    start = time.time()

    expected = os.environ.get("TRANSLATE_TOKEN")
    if expected:
        token = request.headers.get("X-Translate-Token", "")
        if not hmac.compare_digest(token, expected):
            return ("Unauthorized", 401)

    if request.method != "POST":
        return ("Method Not Allowed", 405)

    try:
        body = request.get_json(silent=True) or {}
        _, meta = run_translate()
        meta["total_seconds"] = time.time() - start
        return (meta, 200)

    except Exception as e:
        logging.exception("Validate and translate crashed")
        return (
            {
                "status": "error",
                "seconds": time.time() - start,
                "error": str(e),
            },
            500,
        )


if __name__ == "__main__":
    # Local run (no HTTP request)
    from dotenv import load_dotenv

    load_dotenv()
    _, meta = run_translate()
    print(meta)
