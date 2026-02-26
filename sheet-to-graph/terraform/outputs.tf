output "function_url" {
  value = google_cloudfunctions2_function.translate.service_config[0].uri
}

output "runtime_service_account" {
  value = google_cloudfunctions2_function.translate.service_config[0].service_account_email
}
