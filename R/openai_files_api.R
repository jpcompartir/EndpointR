oai_file_list <- function(purpose = "batch", key_name = "OPENAI_API_KEY") {

  api_key <- get_api_key(key_name)

  httr2::request("https://api.openai.com/v1/files") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_url_query(purpose = purpose) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

}

oai_file_delete <- function(file_id, key_name = "OPENAI_API_KEY") {

  api_key <- get_api_key(key_name)

  httr2::request(paste0("https://api.openai.com/v1/files/", file_id)) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_method("DELETE") |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
