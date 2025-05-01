# Core functions that will be re-used with different providers.

base_request <- function(endpoint_url, api_key){
  # let other functions handle the input checks for now.

  req <-  httr2::request(endpoint_url) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_method("POST") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_bearer_token(token = api_key)

  return(req)
}

#  WIP - deal with ugly chunk in embed_batch_df ----
perform_requests_with_strategy <- function(requests,
                                           indices,
                                           tidy_func,
                                           concurrent_requests = 1,
                                           progress = TRUE) {


  process_response <- function(resp, indices) {
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
        return(create_error_tibble(indices, conditionMessage(e)))
      })
    } else {
      cli::cli_warn("Request failed: {conditionMessage(resp)}")
      return(create_error_tibble(indices, "Request failed"))
    }
  }

  create_error_tibble <- function(indices, error_message) {
    # for consistent outputs
    tibble::tibble(
      embedding = rep(list(NA), length(indices)),
      original_index = indices,
      .error = TRUE,
      .error_message = error_message
    )
  }

  if (concurrent_requests > 1 && length(requests) > 1) {
    responses <- httr2::req_perform_parallel(
      requests,
      on_error = "continue",
      progress = progress,
      max_active = concurrent_requests
    )

    return(purrr::map2(responses, indices, process_response))
  }

  else { # use sequential.
    return(purrr::map2(
      requests,
      indices,
      function(req, idx) {
        tryCatch({
          resp <- httr2::req_perform(req)
          process_response(resp, idx)
        }, error = function(e) {
          cli::cli_warn("Error in sequential request: {conditionMessage(e)}")
          create_error_tibble(idx, conditionMessage(e))
        })
      },
      .progress = progress
    ))
  }
}

