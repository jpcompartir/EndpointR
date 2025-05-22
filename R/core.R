# core functions that will be re-used with different providers.
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
    cli::cli_alert_warning("Sequential request failed: {conditionMessage(e)}")
    return(e)
  })
}


#  WIP - deal with ugly chunk in embed_batch_df ----
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
    cli::cli_alert_info("Performing {length(requests)} requests in parallel (with {concurrent_requests} concurrent requests)...")
    responses <- httr2::req_perform_parallel(
      requests,
      on_error = "continue",
      progress = progress,
      max_active = concurrent_requests
    )
  }

  else { # use sequential.
    cli::cli_alert_info("Performing {length(requests)} requests sequentially...")
    responses <- purrr::map(
      requests,
      perform_request_or_return_error,
      .progress = progress
    )
  }
  return(responses)
}

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

# .create_error_tibble <- function(indices, error_message) {
#
#   tibble::tibble(
#     response = rep(list(NA), length(indices)),
#     original_index = indices,
#     .error = TRUE,
#     .error_message = error_message
#   )
# }

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
