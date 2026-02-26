output "function_url" {
  value = google_cloudfunctions2_function.publisher.service_config[0].uri
}

output "runtime_service_account_email" {
  value = local.function_sa_email
}

output "publish_token_secret_id" {
  value = google_secret_manager_secret.publish_token.secret_id
}
