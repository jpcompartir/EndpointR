  base_request <- function(endpoint_url, api_key){
    # let other functions handle the input checks for now.

   req <-  httr2::request(endpoint_url) |>
      httr2::req_user_agent("EndpointR") |>
      httr2::req_method("POST") |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_auth_bearer_token(token = api_key)

   return(req)
  }

  # hf_build_request docs ----
  #' Prepare a single text embedding request
  #'
  #' @description
  #' Creates an httr2 request object for obtaining a response from a Hugging Face
  #' Inference endpoint for a single text input. The function can be used for
  #' multiple tasks, i.e. for embedding an input, or classifying an input
  #'
  #' @details
  #' For developers, this function can form the basis of single requests, or a
  #' if mapped over a list of requests.
  #'
  #'
  #' @param input Character string to get a response for
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
  #'   req <- hf_build_request(
  #'     input = "This is a sample text to embed",
  #'     endpoint_url = "https://my-endpoint.huggingface.cloud/embedding_api",
  #'     key_name = "HF_API_KEY"
  #'   )
  #'
  #'   # Using default key name
  #'   req <- hf_build_request(
  #'     input = "This is a sample text to classify",
  #'     endpoint_url = "https://my-endpoint.huggingface.cloud/classification_api"
  #'   )
  #' }
  # hf_build_request docs ----
  hf_build_request <- function(input,
                               endpoint_url,
                               key_name,
                               max_retries = 5,
                               timeout = 10,
                               validate = FALSE) {
    stopifnot(
      "text must be provided" = !is.null(input) && nchar(input) > 0,
      "text must be a character string" = is.character(input),
      "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
      "endpoint_url must be a character string" = is.character(endpoint_url),
      "max_retries must be a positive integer" = is.numeric(max_retries) && max_retries > 0,
      "timeout must be a positive number" = is.numeric(timeout) && timeout > 0
    )

    api_key <- get_api_key(key_name)

    if (validate) {
      validate_hf_endpoint(endpoint_url, key_name)
    }

    req <- base_request(endpoint_url, api_key)
    req <- req |>
      httr2::req_body_json(list(inputs = input)) |>
      httr2::req_timeout(timeout) |>
      httr2::req_retry(max_tries = max_retries,
                       backoff = ~ 2 ^ .x, # exponential backoff strategy
                       retry_on_failure = TRUE)

    return(req)
  }

  # hf_build_request_docs ----
  #' Prepare a batch request for multiple texts
  #'
  #' @description
  #' Creates an httr2 request object for obtaining a response from a Hugging Face
  #' Inference endpoint for multiple text inputs in a single batch. This function
  #' can be used for various tasks, such as embedding or classifying multiple inputs
  #' simultaneously.
  #'
  #' @details
  #' For developers, this function forms the basis of batch requests, enabling
  #' more efficient processing of multiple inputs in a single API call.
  #'
  #' @param inputs Vector or list of character strings to process in a batch
  #' @param endpoint_url The URL of the Hugging Face Inference API endpoint
  #' @param key_name Name of the environment variable containing the API key
  #' @param max_retries Maximum number of retry attempts for failed requests
  #' @param timeout Request timeout in seconds
  #' @param validate Whether to validate the endpoint before creating the request
  #'
  #' @return An httr2 request object configured for batch processing
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #'   # Create batch request using API key from environment
  #'   batch_req <- hf_build_request_batch(
  #'     inputs = c("First text to embed", "Second text to embed"),
  #'     endpoint_url = "https://my-endpoint.huggingface.cloud/embedding_api",
  #'     key_name = "HF_API_KEY"
  #'   )
  #'
  #'   # Using custom timeout and retry settings
  #'   batch_req <- hf_build_request_batch(
  #'     inputs = c("Text one", "Text two", "Text three"),
  #'     endpoint_url = "https://my-endpoint.huggingface.cloud/embedding_api",
  #'     key_name = "HF_API_KEY",
  #'     max_retries = 3,
  #'     timeout = 15
  #'   )
  #' }
  # hf_build_request_docs ----
  hf_build_request_batch <- function(inputs, endpoint_url, key_name, max_retries = 5, timeout = 10, validate = FALSE) {

    stopifnot("`inputs` must be a list of inputs" = inherits(inputs, "list")|is.vector(inputs),
              "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
              "endpoint_url must be a character string" = is.character(endpoint_url),
              "max_retries must be a positive integer" = is.numeric(max_retries) && max_retries > 0,
              "timeout must be a positive number" = is.numeric(timeout) && timeout > 0)

    api_key <- get_api_key(key_name)

    req <- base_request(
      endpoint_url = endpoint_url,
      api_key = api_key)

    # for embeddings, should be able to tidy this with tidy_embedding_response()
    req <- req |>
      httr2::req_body_json(list(inputs = inputs)) |>
      httr2::req_timeout(timeout) |>
      httr2::req_retry(max_tries = max_retries, backoff = ~2 ^ .x, retry_on_failure = TRUE)

    return(req)
  }

  # build_request_df docs ----
  #' Prepare embedding requests for texts in a data frame
  #'
  #' @description
  #' Creates httr2 request objects for each text in a data frame column.
  #' Thus function handles request creation, it does not handle performing the request,
  #' or tidying the response. To perform the request, select the appropriate
  #' `*_perform_*` function.
  #'
  #' @param df A data frame containing texts to embed
  #' @param text_var Name of the column containing text to send to the endpoint
  #' @param id_var Name of the column to use as ID (optional)
  #' @param endpoint_url The URL of the Hugging Face Inference API endpoint
  #' @param key_name Name of the environment variable containing the API key
  #' @param max_retries Maximum number of retry attempts for failed requests
  #' @param timeout Request timeout in seconds
  #' @param validate Whether to validate the endpoint before creating requests
  #'
  #' @return A data frame with the original data plus request objects
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #'   # Prepare requests for a data frame
  #'   df <- data.frame(
  #'     id = 1:3,
  #'     text = c("First example", "Second example", "Third example")
  #'   )
  #'
  #'   requests_df <- hf_build_request_df(
  #'     df = df,
  #'     text_var = text,
  #'     endpoint_url = "https://my-endpoint.huggingface.cloud",
  #'     id_var = id
  #'   )
  #' }
  # build_request_df docs ----
  hf_build_request_df <- function(df,
                                  text_var,
                                  id_var,
                                  endpoint_url,
                                  key_name,
                                  max_retries = 3,
                                  timeout = 10,
                                  validate = FALSE) {

    text_sym <- rlang::ensym(text_var)
    id_sym <- rlang::ensym(id_var)

    stopifnot(
      "df must be a data frame" = is.data.frame(df),
      "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0
    )

    if (!rlang::as_string(text_sym) %in% names(df)) {
      cli::cli_abort("Column {.code {rlang::as_string(text_sym)}} not found in data frame")
    }

    if (!rlang::as_string(id_sym) %in% names(df)) {
      cli::cli_abort("Column {.code {rlang::as_string(id_sym)}} not found in data frame")
    }

    api_key <- get_api_key(key_name)

    if (validate) {
      validate_hf_endpoint(endpoint_url, key_name)
    }

    # create requests for each text in the data frame
    result_df <- df |>
      dplyr::mutate(
        .request = purrr::map(
          !!text_sym,
          ~ hf_build_request(
            input = .x,
            endpoint_url = endpoint_url,
            key_name = key_name,
            max_retries = max_retries,
            timeout = timeout,
            validate = FALSE  # already validated if needed
          )
        )
      )

    return(result_df)
  }

  # hf_perform_request_docs ----
  #' Execute a single embedding request and process the response
  #'
  #' @description
  #' Performs a prepared request and returns the response
  #'
  #' @param request An httr2 request object created by hf_build_request
  #' @param ... ellipsis is sent to `httr2::req_perform`, e.g. for `path` and `verbosity`arguments.
  #'
  #' @return A httr2 response object
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #'   # Create and perform request
  #'   req <- hf_build_request(
  #'     input = "This is a sample text to embed",
  #'     endpoint_url = "https://my-endpoint.huggingface.cloud"
  #'   )
  #'   embeddings <- hf_perform_request(req)
  #'
  #' }
  # hf_perform_request_docs ----
  hf_perform_request <- function(request, ...) {
    stopifnot(
      "request must be an httr2 request object" = inherits(request, "httr2_request")
    )

    resp <- httr2::req_perform(request, ...)

    return(resp)
  }

  # hf_perform_sequential_df docs ----
  #' Safely execute multiple embedding requests located in a data frame sequentially
  #'
  #' @description
  #' Performs multiple prepared embedding requests sequentially and returns the responses.
  #' This function will often be considerably slower than sending requests in parallel.
  #' It is left to the user to decide whether they wish to proceed sequentially or in parallel.
  #' For example, `hf_embed_df` has arguments which determine the strategy.
  #'
  #' @details
  #' 'Safely' means that we are using purrr::safely to return information about errors
  #' when they occur, and to continue on to the next request.
  #'
  #'
  #' @param df A data frame with request objects in a column named '.request'
  #' @param progress Whether to display a progress bar
  #'
  #' @return The input data frame with response objects added in '.response' column
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #'   # Prepare requests
  #'   df <- data.frame(
  #'     id = 1:3,
  #'     text = c("First example", "Second example", "Third example")
  #'   )
  #'
  #'   requests_df <- hf_build_request_df(
  #'     df = df,
  #'     text_var = text,
  #'     endpoint_url = "https://my-endpoint.huggingface.cloud"
  #'   )
  #'
  #'   # Execute requests sequentially
  #'   responses_df <- hf_perform_sequential_df(
  #'     df = requests_df,
  #'     progress = TRUE
  #'   )
  #' }
  # hf_perform_sequential_df docs ----
  hf_perform_sequential_df <- function(df, progress = TRUE) {

    stopifnot(
      "df must be a data frame" = is.data.frame(df),
      ".request column must exist in df" = ".request" %in% names(df) # this structure may change, TODO confirm
    )

    result_df <- df |>
      dplyr::mutate(.response = purrr::map(
        .request,
        ~ safely_perform_request(.x),
        .progress = progress
      ))

    return(result_df)
  }


  # hf_perform_parallel_df docs ----
  #' Execute multiple embedding requests in parallel
  #'
  #' @description
  #' Performs multiple prepared embedding requests in parallel and returns the responses.
  #'
  #' @param df A data frame with request objects in a column named '.request'
  #' @param max_active Maximum number of concurrent requests
  #' @param progress Whether to display a progress bar
  #'
  #' @return The input data frame with response objects added in '.response' column
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #'   # Prepare requests
  #'   df <- data.frame(
  #'     id = 1:3,
  #'     text = c("First example", "Second example", "Third example")
  #'   )
  #'
  #'   requests_df <- hf_build_request_df(
  #'     df = df,
  #'     text_var = text,
  #'     endpoint_url = "https://my-endpoint.huggingface.cloud"
  #'   )
  #'
  #'   # Execute requests in parallel
  #'   responses_df <- hf_perform_parallel_df(
  #'     df = requests_df,
  #'     max_active = 5,
  #'     progress = TRUE
  #'   )
  #' }
  # hf_perform_parallel_df docs ----
  hf_perform_parallel_df <- function(df, max_active = 10, progress = TRUE) {
    # the hf_perform functions should probably just become hf_perform
    # they don't really care if the task is embeddings or classification ( or other) TODO
    # the correct tidy/processing function needs to be applied depending on the task.
    stopifnot(
      "df must be a data frame" = is.data.frame(df),
      ".request column must exist in df" = ".request" %in% names(df),
      "max_active must be a positive integer" = is.numeric(max_active) && max_active > 0
    )

    responses <- httr2::req_perform_parallel(
      reqs = df$.request,
      max_active = max_active,
      progress = progress
    )

    imitate_safely_structure <- purrr::map(responses, function(resp) {
      if (inherits(resp, "error")) {
        list(result = NULL, error = resp)
      } else {
        list(result = resp, error = NULL)
      }
    })

    result_df <- df |> dplyr::mutate(.response = imitate_safely_structure)

    return(result_df)
  }





  # utilty for `perform_requests_with_strategy`
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

  # utilty for `perform_requests_with_strategy`
  create_error_tibble <- function(indices, error_message) {

    # for consistent outputs
    tibble::tibble(
      embedding = rep(list(NA), length(indices)),
      original_index = indices,
      .error = TRUE,
      .error_message = error_message
    )
  }

  #  WIP - deal with ugly chunk in embed_df----
  perform_requests_with_strategy <- function(requests, indices, tidy_func,
                                             concurrent_requests = 1,
                                             progress = TRUE) {

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
