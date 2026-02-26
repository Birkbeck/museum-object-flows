output "function_name" {
  value = google_cloudfunctions2_function.flask.name
}

output "function_url" {
  value = google_cloudfunctions2_function.flask.service_config[0].uri
}

output "source_bucket" {
  value = google_storage_bucket.source.name
}
