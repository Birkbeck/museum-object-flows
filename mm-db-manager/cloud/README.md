# MM DB Cloud

This directory contains the Google Cloud–backed operations service for the Museum Database manager.

It provides HTTP endpoints (/add, /edit, /trash, /restore, /delete_permanent) that perform writes to Google Sheets using Google APIs, while keeping the sheets read-only for humans.

It also contains unit tests and integration tests that can be run locally against a real Google Sheet before deployment.

## Google Cloud setup

1. Create / select a Google Cloud project

```
gcloud projects list
gcloud config set project YOUR_PROJECT_ID
```

Enable required APIs:

```
gcloud services enable sheets.googleapis.com
gcloud services enable drive.googleapis.com
```

2. Create a service account for Sheets access

This account is used only by the cloud service, not by humans.

```
gcloud iam service-accounts create sheets-bot \
  --display-name="Sheets DB Bot"
```


The email will look like:

```
sheets-bot@YOUR_PROJECT_ID.iam.gserviceaccount.com
```

3. Share the Google Sheet with the service account
- Open your test spreadsheet in Google Sheets
- Click Share
- Add the service account email
- Give it Editor access
- Explicitly allow the service account to edit any required protected ranges

## Local development setup

1. Install dependencies

From `cloud/`:

```
uv sync
```

2. Authenticate locally (Application Default Credentials)

For local development and tests, use your own Google account:

```
gcloud auth application-default revoke
gcloud auth application-default login \
  --scopes=https://www.googleapis.com/auth/cloud-platform,https://www.googleapis.com/auth/spreadsheets,https://www.googleapis.com/auth/drive
```

This creates:

```
~/.config/gcloud/application_default_credentials.json
```


Your Google account must also have access to the test spreadsheet.

3. Create a local file following the example in `.env.example`

## Running the service locally

The same Flask app used in Cloud Run can be run locally.

Start the local server

```
cd cloud
export HMAC_SECRET="dev-secret"
uv run python devserver.py
```

The service will be available at:

```
http://127.0.0.1:8080
```

## Running tests

### Unit tests only

```
uv run pytest tests/unit
```

These do not hit Google APIs.

### Integration tests (real Google Sheet)

Integration tests:

- call the local HTTP service
- perform real writes to the test spreadsheet
- verify results by reading the sheet

Make sure:

- the local server is running
- ADC is authenticated
- the sheet is shared correctly

Then run:

```
uv run pytest tests/integration
```

## Deployment (Cloud Run)

The production entrypoint is `main.py`.

Deploy:

```
gcloud run deploy mm-db-cloud \
  --source . \
  --region europe-west1 \
  --allow-unauthenticated \
  --set-env-vars HMAC_SECRET=your-prod-secret \
  --service-account sheets-bot@YOUR_PROJECT_ID.iam.gserviceaccount.com
```

After deployment, update:

```
MM_DB_CLOUD_BASE_URL=https://your-cloud-run-url
```
