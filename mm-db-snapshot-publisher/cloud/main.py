import hmac
import os
import time

import pandas as pd
from google.cloud import storage

from mm_db_snapshot_publisher import (
    GoogleUtils,
    MuseumSearchPreprocessor,
    PostcodeGeoDB,
)
from mm_db_snapshot_publisher.cloud_storage_utils import (
    _gcs_bucket,
    _upload_bytes,
    _upload_df_csv,
)


def _resolve_postcode_sqlite_path() -> str:
    """
    Resolve the local sqlite path.

    Behaviour:
    - If POSTCODE_GEO_DB is set, use it.
    - Otherwise default to /tmp/postcode_lookup.sqlite.
    - If the file does not exist locally and POSTCODE_GEO_DB_GCS_URI is set,
      download it from GCS.
    """
    local_path = os.environ.get("POSTCODE_GEO_DB", "/tmp/postcode_lookup.sqlite")
    if os.path.exists(local_path):
        return local_path
    gcs_uri = os.environ.get("POSTCODE_GEO_DB_GCS_URI")
    if not gcs_uri:
        raise RuntimeError(
            f"SQLite DB not found at {local_path} and "
            "POSTCODE_GEO_DB_GCS_URI not set."
        )
    if not gcs_uri.startswith("gs://"):
        raise ValueError("POSTCODE_GEO_DB_GCS_URI must start with gs://")
    bucket_name, blob_name = gcs_uri[5:].split("/", 1)
    client = storage.Client()
    bucket = client.bucket(bucket_name)
    blob = bucket.blob(blob_name)
    blob.download_to_filename(local_path)
    return local_path


def publish(request):
    """
    Cloud Function entrypoint (HTTP).
    """
    start = time.time()

    expected = os.environ.get("PUBLISH_TOKEN")
    if expected:
        token = request.headers.get("X-Publish-Token", "")
        if not hmac.compare_digest(token, expected):
            return ("Unauthorized", 401)
    if request.method != "POST":
        return ("Method Not Allowed", 405)

    spreadsheet_id = os.environ["MAPPING_MUSEUMS_SPREADSHEET_ID"]
    database_tab = os.environ["MAPPING_MUSEUMS_DATABASE_TAB"]

    museums_data = GoogleUtils.read_sheet_to_df(spreadsheet_id, database_tab).rename(
        columns={"id": "museum_id", "name": "museum_name"}
    )

    with PostcodeGeoDB(_resolve_postcode_sqlite_path()) as p:
        geo_df = (
            museums_data["postcode"].fillna("").map(p.get_geo_info).apply(pd.Series)
        )

    museums_data = pd.concat([museums_data, geo_df], axis=1).fillna("")

    search_preprocessor = MuseumSearchPreprocessor.setup(
        museums_data,
        [
            "museum_name",
            "governance",
            "size",
            "subject",
            "accreditation",
            "region",
            "address_1",
            "address_2",
            "address_3",
            "village_town_city",
            "postcode",
            "lad",
            "notes",
        ],
        ngram_range=(1, 2),
    )

    search_structures = search_preprocessor.vectorize_museums()
    X = search_structures["matrix"]
    ids = search_structures["ids"]
    vocab = search_structures["vocab"]
    idf = search_structures["idf"]

    # ---- GCS outputs ----
    bucket, cache_control = _gcs_bucket()
    timestamp = time.strftime("%Y-%m-%d_%H-%M-%S")

    # Stable "latest" paths (Shiny can always read these)
    latest_base = "latest"
    # Versioned snapshot paths (nice for history/rollback)
    versioned_base = "snapshots"

    urls = {}

    # TF-IDF matrix as MatrixMarket text
    mtx_bytes = MuseumSearchPreprocessor.sparse_to_mtx_bytes(X)
    urls["tfidf_mtx"] = _upload_bytes(
        bucket,
        f"{latest_base}/tfidf.mtx",
        mtx_bytes,
        content_type="text/plain; charset=utf-8",
        cache_control=cache_control,
    )
    urls["museum_ids"] = _upload_df_csv(
        bucket,
        f"{latest_base}/museum_ids.csv",
        pd.DataFrame({"museum_id": pd.Series(ids).fillna("").astype(str)}),
        cache_control=cache_control,
    )
    urls["idf"] = _upload_df_csv(
        bucket,
        f"{latest_base}/idf.csv",
        pd.DataFrame({"idf": idf}),
        cache_control=cache_control,
    )
    urls["vocab"] = _upload_df_csv(
        bucket,
        f"{latest_base}/vocab.csv",
        pd.DataFrame({"vocab": vocab}),
        cache_control=cache_control,
    )

    urls["museums_latest"] = _upload_df_csv(
        bucket,
        f"{latest_base}/museums.csv",
        museums_data,
        cache_control=cache_control,
    )
    urls["museums_snapshot"] = _upload_df_csv(
        bucket,
        f"{versioned_base}/museums_{timestamp}.csv",
        museums_data,
        cache_control=cache_control,
    )

    elapsed = time.time() - start
    return (
        {
            "status": "success",
            "seconds": elapsed,
            "rows": int(len(museums_data)),
            "bucket": bucket.name,
            "timestamp": timestamp,
            "urls": urls,
        },
        200,
    )
