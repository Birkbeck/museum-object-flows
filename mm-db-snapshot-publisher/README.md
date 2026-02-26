# MM DB Snapshot Publisher

The application in this repository manages snapshots of the Mapping Museums Database by accessing the spreadsheet where the database is stored and saving a timestamped copy of the data as well as an updated "live version" of the data as a csv in Google Drive. It also generates a TF-IDF matrix used for free-text search of the database and joins geographical information to the data.

## Running locally

To run locally, sign in to the Google Account with access to the spreadsheet and drive folders.

```
gcloud auth application-default login
```

Run:

```
cd cloud
uv run run_local.py
```

## Deployment

Make sure there is a file `terraform/terraform.tfvars` containing:
```
project_id            = "THE_GOOGLE_CLOUD_PROJECT_ID"
region                = "eg us-central1"
geo_bucket_name       = "the name of the bucket where the geo-database is saved"
environment_variables = {
  MAPPING_MUSEUMS_SPREADSHEET_ID = "..."
  MAPPING_MUSEUMS_DATABASE_TAB   = "..."
  POSTCODE_GEO_DB         = "/tmp/postcode_lookup.sqlite"
  POSTCODE_GEO_DB_GCS_URI = "..."
  TFIDF_MTX_FILE_ID        = "..."
  TFIDF_MUSEUM_IDS_FILE_ID = "..."
  TFIDF_VOCAB_FILE_ID      = "..."
  TFIDF_IDF_FILE_ID        = "..."
  MUSEUMS_FILE_ID          = "..."
  MM_DB_SNAPSHOTS          = "..."
}

```

After any changes to the code in `cloud`, re-deploy by navigating to `terraform` and running the deploy command:
```
cd terraform
terraform apply
```

After any changes to the code in `apps-script`, follow instructions in `apps-script/README.md` to redeploy to the Apps Script project.

## Changing the secret

The cloud-run Flask app uses an HMAC secret to prevent unauthorised use of the app. To generate a new secret, run:
```
python generate-secret.py
```

Copy the generated string and store it in both the Apps Script project and the Google Cloud project.

In Apps Script, go to Apps Script → Project Settings → Script Properties → Add:
```
API_BASE_URL = URL_GENERATED_BY_TERRAFORM (should only change if you deploy to a new account)
API_HMAC_SECRET = YOUR_GENERATED_SECRET
```

Update the secret for the google cloud project.

Create/update the secret in secret manager:
```
gcloud secrets create mm-db-snapshot-publisher-publish-token --replication-policy=automatic 2>/dev/null || true
printf '%s' "YOUR_GENERATED_SECRET" | gcloud secrets versions add mm-db-snapshot-publisher-publish-token --data-file=-
```



