#' List files available in the OpenAI Files API
#'
#' @param purpose The intended purpose of the uploaded file, one of "batch", "fine-tune", "assistants", "vision", "user_data", "evals"
#' @param key_name The name of your API key, usually "OPENAI_API_KEY"
#'
#' @returns
#'
#' @export
#' @examples
oai_file_list <- function(purpose = c("batch", "fine-tune", "assistants", "vision", "user_data", "evals"), key_name = "OPENAI_API_KEY") {

  purpose <- match.arg(purpose)
  api_key <- get_api_key(key_name)

  httr2::request("https://api.openai.com/v1/files") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_url_query(purpose = purpose) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

}

#' Delete a file from the OpenAI Files API
#'
#' @param file_id ID of the file given by OpenAI
#' @param key_name The name of your API key, usually "OPENAI_API_KEY"
#'
#' @returns
#'
#' @export
#' @examples
oai_file_delete <- function(file_id, key_name = "OPENAI_API_KEY") {

  api_key <- get_api_key(key_name)

  httr2::request(paste0("https://api.openai.com/v1/files/", file_id)) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_method("DELETE") |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

#' Retrieve content from a file on the OpenAI Files API
#'
#' @param file_id ID of the file given by OpenAI
#' @param key_name The name of your API key, usually "OPENAI_API_KEY"
#'
#' @returns
#'
#' @export
#' @examples
oai_file_content <- function(file_id, key_name = "OPENAI_API_KEY") {

  api_key <- get_api_key(key_name)

  resp <- httr2::request(paste0("https://api.openai.com/v1/files/", file_id, "/content")) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform()

  httr2::resp_body_string(resp)
}
