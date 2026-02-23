############################################################
# Core project / deployment settings
############################################################

variable "project_id" {
  type        = string
  description = "GCP project ID."
}

variable "region" {
  type        = string
  default     = "us-central1"
  description = "Region to deploy the Cloud Function into."
}

variable "function_name" {
  type        = string
  default     = "sheet-to-graph-translate"
  description = "Name of the Cloud Function."
}

variable "source_dir" {
  type        = string
  description = "Directory containing translate.py and sheet_to_graph/."
}

variable "runtime" {
  type        = string
  default     = "python312"
  description = "Cloud Functions runtime."
}

variable "entry_point" {
  type        = string
  default     = "translate"
  description = "Cloud Function entry point (def translate(request))."
}

variable "memory" {
  type        = string
  default     = "2Gi"
  description = "Memory allocation for the function."
}

variable "cpu" {
  type        = string
  default     = "1"
  description = "CPU allocation for the function."
}

variable "timeout_seconds" {
  type        = number
  default     = 540
  description = "Function timeout in seconds."
}

variable "min_instance_count" {
  type        = number
  default     = 0
}

variable "max_instance_count" {
  type        = number
  default     = 1
}

variable "ingress_settings" {
  type        = string
  default     = "ALLOW_ALL"
}

variable "allow_unauthenticated" {
  type        = bool
  default     = true
}

variable "function_service_account_email" {
  type        = string
  default     = ""
  description = "Optional override for runtime service account."
}

############################################################
# Environment variables (sheet config etc.)
############################################################

variable "environment_variables" {
  type        = map(string)
  default     = {}
  description = "Environment variables passed to the function."
}

############################################################
# Postcode Geo SQLite DB (GCS-backed)
############################################################

variable "postcode_geo_db_local_path" {
  type        = string
  default     = "/tmp/postcode_lookup.sqlite"
  description = "Local path inside Cloud Function for the SQLite DB."
}

variable "postcode_geo_bucket_name" {
  type        = string
  description = "Bucket containing the postcode SQLite DB (for IAM grant)."
}

variable "postcode_geo_db_gcs_uri" {
  type        = string
  description = "Full gs:// URI for the postcode SQLite DB."
}

############################################################
# Output CSV destination (GCS)
############################################################

variable "output_bucket_name" {
  type        = string
  description = "GCS bucket where translated CSVs are written."
}

variable "public_cache_control" {
  type        = string
  default     = "public, max-age=300"
  description = "Cache-Control header for uploaded CSVs."
}

variable "make_outputs_public" {
  type        = bool
  default     = true
  description = "If true, grants allUsers objectViewer on the output bucket."
}
