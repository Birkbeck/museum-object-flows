import os

from google.cloud import storage
import pandas as pd


def _gcs_bucket() -> tuple[storage.Bucket, str, str]:
    bucket_name = os.environ["SNAPSHOT_BUCKET"]  # required
    cache_control = os.environ.get("PUBLIC_CACHE_CONTROL", "public, max-age=300")
    client = storage.Client()
    bucket = client.bucket(bucket_name)
    return bucket, cache_control


def _upload_bytes(
    bucket: storage.Bucket,
    object_name: str,
    data: bytes,
    content_type: str,
    cache_control: str,
) -> str:
    blob = bucket.blob(object_name)
    blob.cache_control = cache_control
    blob.upload_from_string(data, content_type=content_type)
    # Public URL format (works if bucket has allUsers objectViewer)
    return f"https://storage.googleapis.com/{bucket.name}/{object_name}"


def _upload_df_csv(
    bucket: storage.Bucket,
    object_name: str,
    df: pd.DataFrame,
    cache_control: str,
) -> str:
    csv_bytes = df.to_csv(index=False).encode("utf-8")
    return _upload_bytes(
        bucket=bucket,
        object_name=object_name,
        data=csv_bytes,
        content_type="text/csv; charset=utf-8",
        cache_control=cache_control,
    )
