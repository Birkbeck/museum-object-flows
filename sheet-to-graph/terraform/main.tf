############################################################
# Project + defaults
############################################################

provider "google" {
  project = var.project_id
  region  = var.region
}

resource "google_project_service" "required" {
  for_each = toset([
    "cloudfunctions.googleapis.com",
    "cloudbuild.googleapis.com",
    "artifactregistry.googleapis.com",
    "run.googleapis.com",
    "storage.googleapis.com",
    "secretmanager.googleapis.com",
    "sheets.googleapis.com",
    "drive.googleapis.com",
  ])

  service            = each.key
  disable_on_destroy = false
}

data "google_project" "this" {
  project_id = var.project_id
}

locals {
  default_function_sa_email = "${data.google_project.this.number}-compute@developer.gserviceaccount.com"
  function_sa_email         = var.function_service_account_email != "" ? var.function_service_account_email : local.default_function_sa_email
}

############################################################
# Source packaging + upload
############################################################

resource "google_storage_bucket" "source" {
  name                        = "${var.project_id}-${var.function_name}-src"
  location                    = var.region
  uniform_bucket_level_access = true

  depends_on = [google_project_service.required]
}

data "archive_file" "source" {
  type        = "zip"
  source_dir  = var.source_dir
  output_path = "${path.module}/${var.function_name}.zip"

  excludes = [
    ".venv",
    "__pycache__",
    ".pytest_cache",
    ".mypy_cache",
    ".ruff_cache",
    ".git",
    ".DS_Store",
  ]
}

resource "google_storage_bucket_object" "source" {
  name   = "${var.function_name}-${data.archive_file.source.output_md5}.zip"
  bucket = google_storage_bucket.source.name
  source = data.archive_file.source.output_path
}

############################################################
# Allow runtime SA to read/write the postcode geo sqlite in GCS
############################################################

resource "google_storage_bucket_iam_member" "postcode_geo_db_rw" {
  bucket = var.postcode_geo_bucket_name
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${local.function_sa_email}"

  depends_on = [google_project_service.required]
}

############################################################
# Allow runtime SA to write translated CSV outputs to GCS
############################################################

resource "google_storage_bucket_iam_member" "outputs_writer" {
  bucket = var.output_bucket_name
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${local.function_sa_email}"

  depends_on = [google_project_service.required]
}

# Optional: Allow public read-only access to output objects
resource "google_storage_bucket_iam_member" "outputs_public_read" {
  count  = var.make_outputs_public ? 1 : 0
  bucket = var.output_bucket_name
  role   = "roles/storage.objectViewer"
  member = "allUsers"

  depends_on = [google_project_service.required]
}

############################################################
# Secret Manager: Translate token (container + IAM)
############################################################

resource "google_secret_manager_secret" "translate_token" {
  secret_id = "${var.function_name}-translate-token"

  replication {
    auto {}
  }

  depends_on = [google_project_service.required]
}

resource "google_secret_manager_secret_iam_member" "translate_token_accessor" {
  secret_id = google_secret_manager_secret.translate_token.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${local.function_sa_email}"

  depends_on = [google_secret_manager_secret.translate_token]
}

############################################################
# Cloud Functions (2nd gen)
############################################################

resource "google_cloudfunctions2_function" "translate" {
  name     = var.function_name
  location = var.region

  build_config {
    runtime     = var.runtime
    entry_point = var.entry_point

    source {
      storage_source {
        bucket = google_storage_bucket.source.name
        object = google_storage_bucket_object.source.name
      }
    }
  }

  service_config {
    available_memory   = var.memory
    available_cpu      = var.cpu
    timeout_seconds    = var.timeout_seconds
    min_instance_count = var.min_instance_count
    max_instance_count = var.max_instance_count
    ingress_settings   = var.ingress_settings

    # Explicitly set runtime SA so IAM bindings are deterministic.
    service_account_email = local.function_sa_email

    environment_variables = merge(
      var.environment_variables,
      {
        # where to find config.json inside the deployed zip
        # sqlite geo db
        POSTCODE_GEO_DB         = var.postcode_geo_db_local_path
        POSTCODE_GEO_DB_GCS_URI = var.postcode_geo_db_gcs_uri
        # output destination (publisher-style)
        SNAPSHOT_BUCKET         = var.output_bucket_name
        PUBLIC_CACHE_CONTROL    = var.public_cache_control
      }
    )

    # Inject Secret Manager secret as env var TRANSLATE_TOKEN (read at runtime)
    secret_environment_variables {
      key        = "TRANSLATE_TOKEN"
      project_id = var.project_id
      secret     = google_secret_manager_secret.translate_token.secret_id
      version    = "latest"
    }
  }

  depends_on = [
    google_project_service.required,
    google_secret_manager_secret_iam_member.translate_token_accessor,
    google_storage_bucket_iam_member.postcode_geo_db_rw,
    google_storage_bucket_iam_member.outputs_writer,
  ]
}

############################################################
# Invoker IAM (optional)
############################################################

resource "google_cloudfunctions2_function_iam_member" "invoker" {
  count          = var.allow_unauthenticated ? 1 : 0
  project        = var.project_id
  location       = var.region
  cloud_function = google_cloudfunctions2_function.translate.name
  role           = "roles/cloudfunctions.invoker"
  member         = "allUsers"
}

resource "google_cloud_run_v2_service_iam_member" "public_invoker" {
  count    = var.allow_unauthenticated ? 1 : 0
  project  = var.project_id
  location = var.region
  name     = var.function_name

  role   = "roles/run.invoker"
  member = "allUsers"
}
