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
    httr2::req_auth_bearer_token(token = api_key)

  return(req)
}


#' Safely perform an embedding request with error handling
#'
#' @description
#' Wrapper around httr2::req_perform that handles errors gracefully.
#'
#' @param request An httr2 request object
#'
#' @return A list with components $result and $error
#' @export
safely_perform_request <- function(request) {
  purrr::safely(httr2::req_perform)(request)
}

perform_request_or_return_error <- function(request) {
  tryCatch({
    response <- httr2::req_perform(request)

    return(response)
  }, error = function(e) {
    cli::cli_alert_warning("Sequential request to {.url {request$url}} failed: {conditionMessage(e)}")
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
#' @details returns responses in the order that requests were sent, and returns errors in a predictable format.
#'
#' @param requests List of httr2_request objects to perform
#' @param concurrent_requests Integer specifying maximum number of simultaneous requests (default: 1)
#' @param progress Logical indicating whether to show progress bar (default: TRUE)
#'
#' @return List of httr2_response objects or error objects for failed requests
#'
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
    cli::cli_alert_info("Performing {length(requests)} request{?s} in parallel (with {concurrent_requests} concurrent requests)...")
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
#'   - .error_message: Character description of any error
#'   - Additional columns from tidy_func output
#'
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
  #higher-order function for processing (takes function as inputs)
  if (inherits(resp, "httr2_response")) {
    tryCatch({
      result <- tidy_func(resp)
      result$original_index <- indices
      result$.error <- FALSE
      result$.error_message <- NA_character_
      return(result)
    }, error = function(e) {
      cli::cli_warn("Error processing response: {conditionMessage(e)}")
      return(.create_error_tibble(indices, conditionMessage(e)))
    })
  } else {
    cli::cli_warn("Request failed: {conditionMessage(resp)}")
    return(.create_error_tibble(indices, "Request failed"))
  }
}

#' Create standardised error tibble for failed requests
#'
#' @description
#' Internal function to create a consistent error tibble structure.
#' Ensures uniform error reporting across different failure modes.
#'
#' @param indices Vector of indices indicating original request positions
#' @param error_message Character string or condition object describing the error
#'
#' @return A tibble with columns:
#'   - original_index: Position in original request batch
#'   - .error: Always TRUE for error tibbles
#'   - .error_message: Character description of the error
#'
#' @keywords internal
.create_error_tibble <- function(indices, error_message) {
  # for consistent outputs with safely function(s)
  if (!is.character(error_message)) {
    if (inherits(error_message, "condition")) {
      error_message <- conditionMessage(error_message)
    } else {
      error_message <- as.character(error_message)
    }
  }

  return(tibble::tibble(
    original_index = indices,
    .error = TRUE,
    .error_message = error_message
  ))
}
