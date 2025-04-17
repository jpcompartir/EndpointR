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

#' Process embedding API response into a tidy format
#'
#' @description
#' Converts the nested list response from a Hugging Face Inference API
#' embedding request into a tidy tibble.
#'
#' @param response An httr2 response object or the parsed JSON response
#'
#' @return A tibble containing the embedding vectors
#' @export
#'
#' @examples
#' \dontrun{
#'   # Process response from httr2 request
#'   req <- hf_embed_request_single(text, endpoint_url, api_key)
#'   resp <- httr2::req_perform(req)
#'   embeddings <- tidy_embedding_response(resp)
#'
#'   # Process already parsed JSON
#'   resp_json <- httr2::resp_body_json(resp)
#'   embeddings <- tidy_embedding_response(resp_json)
#' }
tidy_embedding_response <- function(response) {
  # Handle both httr2 response objects and parsed JSON
  if (inherits(response, "httr2_response")) {
    resp_json <- httr2::resp_body_json(response)
  } else {
    resp_json <- response
  }

  # Handle different response formats from Hugging Face
  if (is.list(resp_json) && !is.null(names(resp_json))) {
    # Some endpoints return {"embedding": [...]} format
    if ("embedding" %in% names(resp_json)) {
      resp_json <- list(resp_json$embedding)
    }
  }

  # Process the nested list into a tibble
  tib <- sapply(resp_json, unlist) |>
    t() |> # transpose to wide form
    as.data.frame.matrix() |>
    tibble::as_tibble()

  return(tib)
}

#' Execute a single embedding request and process the response
#'
#' @description
#' Performs a prepared embedding request and returns the embedding
#' vector in a tidy format.
#'
#' @param request An httr2 request object created by hf_embed_request_single
#' @param tidy Whether to convert the response to a tidy tibble
#'
#' @return A tibble with embedding vectors if tidy=TRUE, otherwise the raw httr2 response
#' @export
#'
#' @examples
#' \dontrun{
#'   # Create and perform request
#'   req <- hf_embed_request_single(
#'     text = "This is a sample text to embed",
#'     endpoint_url = "https://my-endpoint.huggingface.cloud"
#'   )
#'   embeddings <- hf_embed_perform_single(req)
#'
#'   # Get raw response instead of processed embeddings
#'   response <- hf_embed_perform_single(req, tidy = FALSE)
#' }
hf_embed_perform_single <- function(request, tidy = TRUE) {
  # Check input
  stopifnot(
    "request must be an httr2 request object" = inherits(request, "httr2_request")
  )

  # Perform the request
  resp <- httr2::req_perform(request)

  # Return raw response or tidy embeddings
  if (tidy) {
    return(tidy_embedding_response(resp))
  } else {
    return(resp)
  }
}

#' Generate embeddings for a single text
#'
#' @description
#' High-level function to generate embeddings for a single text string.
#' This function handles the entire process from request creation to
#' response processing.
#'
#' @param text Character string to get embeddings for
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param timeout Request timeout in seconds
#' @param validate Whether to validate the endpoint before creating the request
#'
#' @return A tibble containing the embedding vectors
#' @export
#'
#' @examples
#' \dontrun{
#'   # Generate embeddings using API key from environment
#'   embeddings <- hf_embed_text(
#'     text = "This is a sample text to embed",
#'     endpoint_url = "https://my-endpoint.huggingface.cloud"
#'   )
#'
#'   # With custom API key environment variable name
#'   embeddings <- hf_embed_text(
#'     text = "This is a sample text to embed",
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     key_name = "MY_CUSTOM_API_KEY"
#'   )
#' }
hf_embed_text <- function(text,
                         endpoint_url,
                         key_name = "HF_API_KEY",
                         max_retries = 3,
                         timeout = 10,
                         validate = FALSE) {
  # build request with the specified parameters
  req <- hf_embed_request_single(
    text = text,
    endpoint_url = endpoint_url,
    key_name = key_name,
    max_retries = max_retries,
    timeout = timeout,
    validate = validate
  )

  # perform request and provide user-friendly error messages
  tryCatch({
    embeddings <- hf_embed_perform_single(req, tidy = TRUE)
    return(embeddings)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to generate embeddings",
      "i" = "Text: {cli::cli_vec(text, list('vec-trunc' = 30, 'vec-sep' = ''))}",
      "x" = "Error: {conditionMessage(e)}"
    ))
  })
}
