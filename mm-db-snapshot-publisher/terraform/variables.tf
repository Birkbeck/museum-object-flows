variable "project_id" {
  type = string
}

variable "region" {
  type    = string
  default = "us-central1"
}

variable "function_name" {
  type    = string
  default = "mm-db-snapshot-publisher"
}

variable "source_dir" {
  type        = string
  description = "Directory containing your function code (main.py, requirements.txt, package, etc.)"
}

variable "runtime" {
  type    = string
  default = "python311"
}

variable "entry_point" {
  type    = string
  default = "publish"
}

variable "cpu" {
  type    = string
  default = "1"
}

variable "memory" {
  type    = string
  default = "2Gi"
}

variable "timeout_seconds" {
  type    = number
  default = 540
}

variable "min_instance_count" {
  type    = number
  default = 0
}

variable "max_instance_count" {
  type    = number
  default = 1
}

variable "ingress_settings" {
  type    = string
  default = "ALLOW_ALL"
}

variable "allow_unauthenticated" {
  type    = bool
  default = true
}

variable "function_service_account_email" {
  type        = string
  description = "Optional: runtime SA email. If empty, uses the default compute SA."
  default     = ""
}

variable "environment_variables" {
  type    = map(string)
  default = {}
}

variable "geo_bucket_name" {
  type        = string
  description = "GCS bucket name that holds the postcode sqlite DB"
}
