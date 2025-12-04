#' Create a base HTTP POST request for API endpoints
#'
#' @description
#' Constructs a base httr2 POST request object with common headers and authentication.
#' This function sets up the foundation for API requests with standard configuration.
#'
#' @param endpoint_url Character string containing the API endpoint URL
#' @param api_key Character string containing the API authentication key
#'
#' @return An httr2_request object configured with POST method, JSON content type, and bearer token authentication
#'
#' @examples
#' \dontrun{
#'   # Create a base POST request for an API endpoint
#'   req <- base_request(
#'     endpoint_url = "https://api.example.com/v1/endpoint",
#'     api_key = "your-api-key-here"
#'   )
#' }
base_request <- function(endpoint_url, api_key){
  # let other functions handle the input checks for now.

  req <-  httr2::request(endpoint_url) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_method("POST") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_bearer_token(token = api_key) |>
    httr2::req_error(is_error = ~ FALSE) # don't let httr2 auto-throw errors; we handle them ourselves for better error messages

  return(req)
}


#' Safely perform an embedding request with error handling
#'
#' @description
#' Wrapper around httr2::req_perform that handles errors gracefully.
#' Returns the response object directly - check status with httr2::resp_status().
#'
#' @param request An httr2 request object
#'
#' @return A list with components $result (httr2_response or NULL) and $error (NULL or condition)
#' @export
safely_perform_request <- function(request) {
  purrr::safely(httr2::req_perform)(request)
}

#' Perform request and return response or error object
#'
#' @description
#' Performs a request and returns the response. Since req_error(is_error = ~ FALSE)
#' is set in base_request(), httr2 won't throw errors for HTTP status codes >= 400.
#' Instead, callers should check the response status with httr2::resp_status().
#'
#' @param request An httr2 request object
#'
#' @return An httr2_response object (check status with resp_status()) or an error condition
#' @keywords internal
perform_request_or_return_error <- function(request) {
  tryCatch({
    response <- httr2::req_perform(request)
    # with req_error(is_error = ~ FALSE), we get responses even for HTTP errors
    # callers should check status themselves
    return(response)
  }, error = function(e) {
    # this catches network errors, timeouts, etc. (not HTTP status errors)
    cli::cli_alert_warning("Request to {.url {request$url}} failed: {conditionMessage(e)}")
    return(e)
  })
}


#' Perform multiple requests with configurable concurrency strategy
#'
#' @description
#' Executes a list of HTTP requests either sequentially or in parallel.
#' Automatically chooses sequential processing when concurrent_requests = 1
#' or when there's only one request.
#'
#' @details Returns responses in the order that requests were sent.
#' Since requests use req_error(is_error = ~ FALSE), HTTP error responses (status >= 400)
#' are returned as httr2_response objects rather than being thrown as errors.
#' Callers should check response status with httr2::resp_status() or use
#' httr2::resps_successes() / httr2::resps_failures() to categorise responses.
#'
#' @param requests List of httr2_request objects to perform
#' @param concurrent_requests Integer specifying maximum number of simultaneous requests (default: 1)
#' @param progress Logical indicating whether to show progress bar (default: TRUE)
#'
#' @return List of httr2_response objects (check status with resp_status()) or error objects for network failures
#' @export
#' @examples
#' \dontrun{
#'   # Sequential processing
#'   responses <- perform_requests_with_strategy(
#'     requests = my_requests,
#'     concurrent_requests = 1
#'   )
#'
#'   # Parallel processing with 5 concurrent requests
#'   responses <- perform_requests_with_strategy(
#'     requests = my_requests,
#'     concurrent_requests = 5,
#'     progress = TRUE
#'   )
#' }
perform_requests_with_strategy <- function(requests,
                                           concurrent_requests = 1,
                                           progress = TRUE) {


  if (!is.list(requests) || !all(purrr::map_lgl(requests, inherits, "httr2_request"))) {
    cli::cli_abort("`requests` must be a list of httr2_request objects.")
  }
  if (!is.numeric(concurrent_requests) || concurrent_requests < 1) {
    cli::cli_abort("`concurrent_requests` must be a positive integer.")
  }

  if (concurrent_requests > 1 && length(requests) > 1) { # use parallel.
    cli::cli_alert_info("Performing {length(requests)} request{?s} in parallel (with {concurrent_requests} concurrent request{?s})...")
    responses <- httr2::req_perform_parallel(
      requests,
      on_error = "continue",
      progress = progress,
      max_active = concurrent_requests
    )
  }

  else { # use sequential.
    cli::cli_alert_info("Performing {length(requests)} request{?s} sequentially...")
    responses <- purrr::map(
      requests,
      perform_request_or_return_error,
      .progress = progress
    )
  }
  return(responses)
}

#' Process API response with error handling
#'
#' @description
#' Higher-order function that applies a tidying function to an API response.
#' Handles both successful responses and errors, returning a consistent tibble structure. The `tidy_func` parameter allows you to provide the necessary function for your particular workflow.
#'
#'
#' @param resp An httr2_response object or error object from a failed request
#' @param indices Vector of indices to track original position of requests
#' @param tidy_func Function to process/tidy successful API responses
#'
#' @return A tibble with processed results or error information, including:
#'   - original_index: Position in original request batch
#'   - .error: Logical indicating if an error occurred
#'   - .error_msg: Character description of any error
#'   - Additional columns from tidy_func output
#'
#' @export
#' @examples
#' \dontrun{
#'   # Process a response with custom tidying function
#'   result <- process_response(
#'     resp = api_response,
#'     indices = c(1, 2, 3),
#'     tidy_func = function(r) { tibble::tibble(data = httr2::resp_body_json(r)) }
#'   )
#' }
process_response <- function(resp, indices, tidy_func) {
  # higher-order function for processing (takes function as inputs)
  if (inherits(resp, "httr2_response")) {
    # check if response is an error (status >= 400)
    status <- httr2::resp_status(resp)
    if (status >= 400) {
      error_msg <- .extract_api_error(resp)
      cli::cli_warn("Request failed with status {status}: {error_msg}")
      return(.create_error_tibble(indices, error_msg, status = status))
    }

    tryCatch({
      result <- tidy_func(resp)
      result$original_index <- indices
      result$.error <- FALSE
      result$.error_msg <- NA_character_
      result$.status <- NA_integer_
      return(result)
    }, error = function(e) {
      cli::cli_warn("Error processing response: {conditionMessage(e)}")
      return(.create_error_tibble(indices, conditionMessage(e)))
    })
  } else {
    # handle non-response objects (e.g., errors from network failures)
    error_msg <- .extract_api_error(resp, "Request failed")
    cli::cli_warn("Request failed: {error_msg}")
    return(.create_error_tibble(indices, error_msg))
  }
}

#' Create standardised error tibble for failed requests
#'
#' @description
#' Internal function to create a consistent error tibble structure.
#' Ensures uniform error reporting across different failure modes.
#'
#' @param indices Vector of indices indicating original request positions
#' @param error_msg Character string or condition object describing the error
#' @param status HTTP status code (integer) or NA_integer_ for non-HTTP errors.
#'   Defaults to NA_integer_.
#'
#' @return A tibble with columns:
#'   - original_index: Position in original request batch
#'   - .error: TRUE for errors
#'   - .error_msg: Character description of the error
#'   - .status: HTTP status code (integer) or NA for non-HTTP errors
#'
#' @keywords internal
.create_error_tibble <- function(indices, error_msg, status = NA_integer_) {
  # for consistent outputs with safely function(s)
  if (!is.character(error_msg)) {
    if (inherits(error_msg, "condition")) {
      error_msg <- conditionMessage(error_msg)
    } else {
      error_msg <- as.character(error_msg)
    }
  }

  return(tibble::tibble(
    original_index = indices,
    .error = TRUE,
    .error_msg = error_msg,
    .status = status
  ))
}


#' Extract error message from an API response
#'
#' @description
#' Extracts a meaningful error message from an httr2 response object.
#' Handles different API response formats (OpenAI, Anthropic, HuggingFace).
#'
#' @param response An httr2_response object, error object, or other response type
#' @param fallback_message Message to return if extraction fails
#'
#' @return Character string containing the error message, or NA_character_ if response is successful
#' @keywords internal
.extract_api_error <- function(response, fallback_message = "Unknown error") {
  # handle non-response objects (e.g., errors from network failures)
  if (!inherits(response, "httr2_response")) {
    if (inherits(response, "error") || inherits(response, "condition")) {
      return(conditionMessage(response))
    }
    return(as.character(fallback_message))
  }

  status <- httr2::resp_status(response)
  if (status < 400) return(NA_character_)

  # try to extract error from response body - different APIs use different formats

  tryCatch({
    body <- httr2::resp_body_json(response)
    # huggingface format: {"error": "..."} - check first as it's a string not a list
    if (!is.null(body$error) && is.character(body$error)) return(body$error)
    # openai format: {"error": {"message": "...", "type": "..."}}
    if (!is.null(body$error) && is.list(body$error) && !is.null(body$error$message)) return(body$error$message)
    # anthropic format: {"message": "..."}
    if (!is.null(body$message)) return(body$message)
    # fallback to status code
    paste("HTTP", status)
  }, error = function(e) {
    paste("HTTP", status)
  })
}
