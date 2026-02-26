############################################################
# Project + defaults
############################################################

resource "google_project_service" "required" {
  for_each = toset([
    "cloudfunctions.googleapis.com",
    "cloudbuild.googleapis.com",
    "artifactregistry.googleapis.com",
    "run.googleapis.com",
    "storage.googleapis.com",
    "secretmanager.googleapis.com",
    "sheets.googleapis.com",
  ])

  service            = each.key
  disable_on_destroy = false
}

data "google_project" "this" {
  project_id = var.project_id
}

locals {
  # Cloud Functions (2nd gen) defaults to the Compute Engine default service account
  # if service_account_email is not set.
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
# Secret Manager (container + IAM + env injection)
############################################################

# Secret "container" only (no secret value in Terraform)
resource "google_secret_manager_secret" "hmac_secret" {
  secret_id = "${var.function_name}-hmac-secret"

  replication {
    auto {}
  }

  depends_on = [google_project_service.required]
}

# Allow the function runtime service account to read the secret
resource "google_secret_manager_secret_iam_member" "hmac_secret_accessor" {
  secret_id = google_secret_manager_secret.hmac_secret.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${local.function_sa_email}"

  depends_on = [google_secret_manager_secret.hmac_secret]
}

############################################################
# Cloud Functions (2nd gen)
############################################################

resource "google_cloudfunctions2_function" "flask" {
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
    timeout_seconds    = var.timeout_seconds
    min_instance_count = var.min_instance_count
    max_instance_count = var.max_instance_count
    ingress_settings   = var.ingress_settings

    # Explicitly set runtime SA so IAM bindings are deterministic.
    service_account_email = local.function_sa_email

    environment_variables = merge(
      var.environment_variables,
      {
        MM_DB_SPREADSHEET_ID = var.mm_db_spreadsheet_id
      }
    )

    # Inject Secret Manager secret as env var HMAC_SECRET (read at runtime)
    secret_environment_variables {
      key        = "HMAC_SECRET"
      project_id = var.project_id
      secret     = google_secret_manager_secret.hmac_secret.secret_id
      version    = "latest"
    }
  }

  depends_on = [
    google_project_service.required,
    google_secret_manager_secret_iam_member.hmac_secret_accessor,
  ]
}

############################################################
# Public invoker (optional)
############################################################

resource "google_cloudfunctions2_function_iam_member" "invoker" {
  count          = var.allow_unauthenticated ? 1 : 0
  project        = var.project_id
  location       = var.region
  cloud_function = google_cloudfunctions2_function.flask.name
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
