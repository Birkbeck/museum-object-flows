# ONSPD to SQLite

Use this script to covert ONS postcode directory CSVs into an SQLite database.

Run with command

```
uv run main.py
```

## Uploading the database to Google Cloud Storage Bucket

Use gcloud to upload the sqlite database:

```
gcloud storage cp postcode_lookup.sqlite gs://museum-analytics-geo-db/onspd/postcode_lookup.sqlite
```

## Setting up a Google Cloud Storage Bucket

If, you are redeploying to a new project, navigate to `terraform` and initialize

```
cd terraform
terraform init
```

When prompted, provide a bucket name (decide a new name - the current name is `museum-analytics-geo-db`) and the cloud project id (where code that needs to access the database is deployed).

Then apply changes:

```
terraform apply
```