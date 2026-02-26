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
  # Cloud Functions (2nd gen) defaults to the Compute Engine default SA if not set.
  default_function_sa_email = "${data.google_project.this.number}-compute@developer.gserviceaccount.com"

  # Allow override; otherwise use default.
  function_sa_email = var.function_service_account_email != "" ? var.function_service_account_email : local.default_function_sa_email
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
# Public snapshots bucket (for Shiny to read)
############################################################

resource "google_storage_bucket" "snapshots" {
  name                        = "mapping-museums-database"
  location                    = var.region
  uniform_bucket_level_access = true

  # Optional but sensible
  versioning {
    enabled = true
  }

  depends_on = [google_project_service.required]
}

# Allow the function runtime SA to write objects
resource "google_storage_bucket_iam_member" "snapshots_writer" {
  bucket = google_storage_bucket.snapshots.name
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${local.function_sa_email}"

  depends_on = [google_storage_bucket.snapshots]
}

# Allow public read-only access to objects
resource "google_storage_bucket_iam_member" "snapshots_public_read" {
  bucket = google_storage_bucket.snapshots.name
  role   = "roles/storage.objectViewer"
  member = "allUsers"

  depends_on = [google_storage_bucket.snapshots]
}

############################################################
# Secret Manager (container + IAM + env injection)
############################################################

# Secret "container" only (no secret value in Terraform)
resource "google_secret_manager_secret" "publish_token" {
  secret_id = "${var.function_name}-publish-token"

  replication {
    auto {}
  }

  depends_on = [google_project_service.required]
}

# Allow the function runtime service account to read the secret
resource "google_secret_manager_secret_iam_member" "publish_token_accessor" {
  secret_id = google_secret_manager_secret.publish_token.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${local.function_sa_email}"

  depends_on = [google_secret_manager_secret.publish_token]
}

############################################################
# Allow runtime SA to read the geo sqlite from a GCS bucket
############################################################

resource "google_storage_bucket_iam_member" "geo_db_reader" {
  bucket = var.geo_bucket_name
  role   = "roles/storage.objectViewer"
  member = "serviceAccount:${local.function_sa_email}"

  depends_on = [google_project_service.required]
}

############################################################
# Cloud Functions (2nd gen)
############################################################

resource "google_cloudfunctions2_function" "publisher" {
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

    # Non-secret env vars (IDs, paths, etc.)
    environment_variables = merge(
      var.environment_variables,
      {
        SNAPSHOT_BUCKET = google_storage_bucket.snapshots.name
      }
    )

    # Inject Secret Manager secret as env var PUBLISH_TOKEN (read at runtime)
    secret_environment_variables {
      key        = "PUBLISH_TOKEN"
      project_id = var.project_id
      secret     = google_secret_manager_secret.publish_token.secret_id
      version    = "latest"
    }
  }

  depends_on = [
    google_project_service.required,
    google_secret_manager_secret_iam_member.publish_token_accessor,
    google_storage_bucket_iam_member.geo_db_reader,
    google_storage_bucket_iam_member.snapshots_writer,
    google_storage_bucket_iam_member.snapshots_public_read,
  ]

}

############################################################
# Public invoker (optional)
############################################################

resource "google_cloudfunctions2_function_iam_member" "invoker" {
  count          = var.allow_unauthenticated ? 1 : 0
  project        = var.project_id
  location       = var.region
  cloud_function = google_cloudfunctions2_function.publisher.name
  role           = "roles/cloudfunctions.invoker"
  member         = "allUsers"
}

# Gen2 runs on Cloud Run; some setups also require Run invoker.
resource "google_cloud_run_v2_service_iam_member" "public_invoker" {
  count    = var.allow_unauthenticated ? 1 : 0
  project  = var.project_id
  location = var.region
  name     = var.function_name

  role   = "roles/run.invoker"
  member = "allUsers"
}
