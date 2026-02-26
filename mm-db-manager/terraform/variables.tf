variable "project_id" {
  type        = string
  description = "GCP project ID."
}

variable "region" {
  type        = string
  description = "GCP region for Cloud Functions."
  default     = "us-central1"
}

variable "function_name" {
  type        = string
  description = "Cloud Function name."
  default     = "mm-db-manager"
}

variable "source_dir" {
  type        = string
  description = "Path to the Flask app source directory."
  default     = "../cloud"
}

variable "runtime" {
  type        = string
  description = "Cloud Functions runtime."
  default     = "python313"
}

variable "entry_point" {
  type        = string
  description = "Entry point for the function (Flask app object)."
  default     = "app"
}

variable "memory" {
  type        = string
  description = "Memory allocation for the function."
  default     = "512Mi"
}

variable "timeout_seconds" {
  type        = number
  description = "Function timeout in seconds."
  default     = 60
}

variable "min_instance_count" {
  type        = number
  description = "Minimum instances to keep warm."
  default     = 0
}

variable "max_instance_count" {
  type        = number
  description = "Maximum instances."
  default     = 2
}

variable "ingress_settings" {
  type        = string
  description = "Ingress settings."
  default     = "ALLOW_ALL"
}

variable "environment_variables" {
  type        = map(string)
  description = "Environment variables for the function."
  default     = {
    PYTHONPATH = "src"
  }
}

variable "allow_unauthenticated" {
  type        = bool
  description = "Allow unauthenticated invocations."
  default     = true
}

variable "function_service_account_email" {
  type        = string
  description = "Service account email used by the Cloud Function at runtime. Leave empty to use the default Compute Engine service account."
  default     = ""
}

variable "mm_db_spreadsheet_id" {
  type        = string
  description = "Pinned spreadsheet ID used by the backend to read/write the DB sheet."
}