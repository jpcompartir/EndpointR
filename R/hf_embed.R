#' Validate a Hugging Face Inference Endpoint
#'
#' @description
#' Checks if an endpoint URL is valid and accessible with the provided API key.
#' This function sends a small test request to verify the endpoint works.
#'
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#'
#' @return logical TRUE if endpoint is valid, otherwise stops with an error
#' @export
#'
#' @examples
#' \dontrun{
#'   # Validate endpoint retrieving API key from environment
#'   validate_hf_endpoint(
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     key_name = "HF_API_KEY"
#'   )
#'
#'   # Using default key name
#'   validate_hf_endpoint("https://my-endpoint.huggingface.cloud")
#' }
validate_hf_endpoint <- function(endpoint_url, key_name = "HF_API_KEY") {
  stopifnot(
    "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
    "endpoint_url must be a character string" = is.character(endpoint_url)
  )

  api_key <- get_api_key(key_name)
  test_text <- "Hello world"

  req <- httr2::request(endpoint_url) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_method("POST") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_bearer_token(token = api_key) |>
    httr2::req_body_json(list(inputs = test_text)) |>
    httr2::req_timeout(10) |> # short timeout to fail fast
    httr2::req_error(body = function(resp) {
      body <- httr2::resp_body_json(resp)
      msg <- body$error %||% "Unknown error occurred"
      return(paste("Endpoint validation failed:", msg))
    })

  # handle both http errors and connection issues separately
  tryCatch({
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) >= 400) {
      cli::cli_abort("Endpoint returned error: {httr2::resp_status_desc(resp)}")
    }
    return(TRUE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Cannot connect to Hugging Face endpoint",
      "i" = "URL: {endpoint_url}",
      "x" = "Error: {conditionMessage(e)}"
    ))
  })
}

#' Prepare a single text embedding request
#'
#' @description
#' Creates an httr2 request object for obtaining embeddings from a Hugging Face
#' Inference endpoint for a single text input.
#'
#' @param text Character string to get embeddings for
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param timeout Request timeout in seconds
#' @param validate Whether to validate the endpoint before creating the request
#'
#' @return An httr2 request object
#' @export
#'
#' @examples
#' \dontrun{
#'   # Create request using API key from environment
#'   req <- hf_embed_request_single(
#'     text = "This is a sample text to embed",
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     key_name = "HF_API_KEY"
#'   )
#'
#'   # Using default key name
#'   req <- hf_embed_request_single(
#'     text = "This is a sample text to embed",
#'     endpoint_url = "https://my-endpoint.huggingface.cloud"
#'   )
#' }
hf_embed_request_single <- function(text,
                                    endpoint_url,
                                    key_name = "HF_API_KEY",
                                    max_retries = 3,
                                    timeout = 10,
                                    validate = FALSE) {
  stopifnot(
    "text must be provided" = !is.null(text) && nchar(text) > 0,
    "text must be a character string" = is.character(text),
    "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
    "endpoint_url must be a character string" = is.character(endpoint_url),
    "max_retries must be a positive integer" = is.numeric(max_retries) && max_retries > 0,
    "timeout must be a positive number" = is.numeric(timeout) && timeout > 0
  )

  api_key <- get_api_key(key_name)

  if (validate) {
    validate_hf_endpoint(endpoint_url, key_name)
  }

  req <- httr2::request(endpoint_url) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_method("POST") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_bearer_token(token = api_key) |>
    httr2::req_body_json(list(inputs = text)) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = max_retries,
                     backoff = ~ 2 ^ .x, # exponential backoff strategy
                     retry_on_failure = TRUE)

  return(req)
}
