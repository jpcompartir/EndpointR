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
#'   req <- hf_build_request(
#'     text = "This is a sample text to embed",
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     key_name = "HF_API_KEY"
#'   )
#'
#'   # Using default key name
#'   req <- hf_build_request(
#'     text = "This is a sample text to embed",
#'     endpoint_url = "https://my-endpoint.huggingface.cloud"
#'   )
#' }
hf_build_request <- function(text,
                                    endpoint_url,
                                    key_name,
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

## Space for batch function `hf_embed_b`
## TODO

#' Execute a single embedding request and process the response
#'
#' @description
#' Performs a prepared embedding request and returns the embedding
#' vector in a tidy format.
#'
#' @param request An httr2 request object created by hf_build_request
#' @param tidy Whether to convert the response to a tidy tibble
#'
#' @return A tibble with embedding vectors if tidy=TRUE, otherwise the raw httr2 response
#' @export
#'
#' @examples
#' \dontrun{
#'   # Create and perform request
#'   req <- hf_build_request(
#'     text = "This is a sample text to embed",
#'     endpoint_url = "https://my-endpoint.huggingface.cloud"
#'   )
#'   embeddings <- hf_perform_request(req)
#'
#'   # Get raw response instead of processed embeddings
#'   response <- hf_perform_request(req, tidy = FALSE)
#' }
hf_perform_request <- function(request, tidy = TRUE) {
  stopifnot(
    "request must be an httr2 request object" = inherits(request, "httr2_request")
  )

  resp <- httr2::req_perform(request)

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
                         key_name,
                         max_retries = 3,
                         timeout = 10,
                         validate = FALSE) {

  # build request with the specified parameters
  req <- hf_build_request(
    text = text,
    endpoint_url = endpoint_url,
    key_name = key_name,
    max_retries = max_retries,
    timeout = timeout,
    validate = validate
  )

  # perform request and provide user-friendly error messages
  tryCatch({
    embeddings <- hf_perform_request(req, tidy = TRUE)
    return(embeddings)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to generate embeddings",
      "i" = "Text: {cli::cli_vec(text, list('vec-trunc' = 30, 'vec-sep' = ''))}",
      "x" = "Error: {conditionMessage(e)}"
    ))
  })
}

#' Prepare embedding requests for texts in a data frame
#'
#' @description
#' Creates httr2 request objects for each text in a data frame column.
#' Thus function handles request creation, it does not handle performing the request,
#' or tidying the response. To perform the request, select the appropriate
#' `*_perform_*` function.
#'
#' @param df A data frame containing texts to embed
#' @param text_var Name of the column containing text to embed
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
          text = .x,
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
#'     text_col = "text",
#'     endpoint_url = "https://my-endpoint.huggingface.cloud"
#'   )
#'
#'   # Execute requests in parallel
#'   responses_df <- hf_perform_parallel(
#'     df = requests_df,
#'     max_active = 5,
#'     progress = TRUE
#'   )
#' }
hf_perform_parallel <- function(df, max_active = 10, progress = TRUE) {
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

  result_df <- df |>
    dplyr::mutate(.response = responses)

  return(result_df)
}

#' Execute multiple embedding requests located in a data frame sequentially
#'
#' @description
#' Performs multiple prepared embedding requests sequentially and returns the responses.
#' This function will often be considerably slower than sending requests in parallel.
#' It is left to the user to decide whether they wish to proceed sequentially or in parallel.
#' For example, `hf_embed_df` has arguments which determine the strategy.
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
#'     text_col = "text",
#'     endpoint_url = "https://my-endpoint.huggingface.cloud"
#'   )
#'
#'   # Execute requests sequentially
#'   responses_df <- hf_perform_sequential(
#'     df = requests_df,
#'     progress = TRUE
#'   )
#' }
hf_perform_sequential <- function(df, progress = TRUE) {

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

#' Split a data frame into chunks for batch processing
#'
#' @description
#' Splits a data frame into chunks of specified size.

#'
#' @param df A data frame to split into batches
#' @param chunk_size Number of rows per batch
#'
#' @return A list of data frames, each with at most chunk_size rows
#' @keywords internal
chunk_dataframe <- function(df, chunk_size) {
  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "chunk_size must be a positive integer" = is.numeric(chunk_size) && chunk_size > 0
  )

  # don't batch if the df is smaller than batch size
  if (nrow(df) <= chunk_size) {
    return(list(df))
  }

  df_chunks <- split(df, ceiling(seq_len(nrow(df)) / chunk_size))
  return(df_chunks)
}

#' Process chunked data frames into embeddings
#'
#' @description
#' Processes chunks of responses into tidy embeddings and handles errors.
#'
#' @param df Data frame with response objects in '.response' column
#' @param include_errors Whether to include rows with errors in the result
#'
#' @return A data frame with the original data plus embedding columns
#' @export
#'
#' @examples
#' \dontrun{
#'   # Process responses into embeddings
#'   embeddings_df <- process_chunked_df(
#'     df = responses_df,
#'     include_errors = FALSE
#'   )
#' }
process_chunked_df <- function(df, include_errors = FALSE) {
  # TODO re-work pending changes to batch -> sequential/parallel

  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    ".response column must exist in df" = ".response" %in% names(df)
  )

  # for parallel responses (list of responses)
  if (all(purrr::map_lgl(df$.response, ~ inherits(.x, "httr2_response")))) {
    result_df <- df |>
      dplyr::mutate(
        .embeddings = purrr::map(.response, ~ tryCatch(
          tidy_embedding_response(.x),
          error = function(e) NULL
        ))
      )
  } else {
    # for purrr::safely wrapped responses (list of lists with result/error)
    result_df <- df |>
      dplyr::mutate(
        .error = purrr::map_lgl(.response, ~ !is.null(.x$error)),
        .error_message = purrr::map_chr(.response, ~ ifelse(
          is.null(.x$error),
          NA_character_,
          as.character(.x$error)
        )),
        .embeddings = purrr::map(.response, ~ tryCatch(
          if (!is.null(.x$result)) tidy_embedding_response(.x$result) else NULL,
          error = function(e) NULL
        ))
      )
  }


  if (!include_errors) {
    if (".error" %in% names(result_df)) {
      result_df <- result_df |>
        dplyr::filter(!.error)
    } else {
      result_df <- result_df |>
        dplyr::filter(!purrr::map_lgl(.embeddings, is.null))
    }
  }

  # take care of unnesting for tidy results, if we can find .embeddings - this structure may change...
  if (".embeddings" %in% names(result_df) &&
      all(!is.null(result_df$.embeddings)) &&
      any(purrr::map_lgl(result_df$.embeddings, ~ inherits(.x, "data.frame") || is.null(.x)))) {

    # get column names from the first non-null embeddings tibble
    non_null_embeddings <- result_df$.embeddings[!purrr::map_lgl(result_df$.embeddings, is.null)]
    if (length(non_null_embeddings) > 0) {
      # unnest embeddings into wide format without adding prefix
      result_df <- result_df |>
        tidyr::unnest(.embeddings)
    }
  }

  result_df <- result_df |>
    # drop cols we don't need anymore.
    dplyr::select(-.request, -.response)

  return(result_df)
}

#' Generate embeddings for texts in a data frame
#'
#' @description
#' High-level function to generate embeddings for texts in a data frame.
#' This function handles the entire process from request creation to
#' response processing, with options for batching & parallel exeuction.
#' Setting the number of retries,
#'
#' @param df A data frame containing texts to embed
#' @param text_var Name of the column containing text to embed
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param id_var Name of the column to use as ID
#' @param key_name Name of the environment variable containing the API key
#' @param batch_size Number of texts to process in one batch (NULL for no batching)
#' @param parallel Whether to execute requests in parallel
#' @param max_active Maximum number of concurrent requests when parallel=TRUE
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param timeout Request timeout in seconds
#' @param progress Whether to display a progress bar
#' @param validate Whether to validate the endpoint before creating requests
#' @param include_errors Whether to include rows with errors in the result
#'
#' @return A data frame with the original data plus embedding columns
#' @export
#'
#' @examples
#' \dontrun{
#'   # Generate embeddings for a data frame
#'   df <- data.frame(
#'     id = 1:3,
#'     text = c("First example", "Second example", "Third example")
#'   )
#'
#'   # Use parallel processing without batching
#'   embeddings_df <- hf_embed_df(
#'     df = df,
#'     text_var = text,
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     id_var = id,
#'     parallel = TRUE,
#'     batch_size = NULL
#'   )
#'
#'   # Use batching without parallel processing
#'   embeddings_df <- hf_embed_df(
#'     df = df,
#'     text_var = text,
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     id_var = id,
#'     parallel = FALSE,
#'     batch_size = 10
#'   )
#'
#'   # Use both batching and parallel processing
#'   embeddings_df <- hf_embed_df(
#'     df = df,
#'     text_var = text,
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     id_var = id,
#'     parallel = TRUE,
#'     batch_size = 10
#'   )
#' }
hf_embed_df <- function(df,
                       text_var,
                       endpoint_url,
                       key_name,
                       id_var,
                       batch_size = NULL,
                       parallel = FALSE,
                       max_active = 10,
                       max_retries = 3,
                       timeout = 10,
                       progress = TRUE,
                       validate = FALSE,
                       include_errors = FALSE) {


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

  if (!is.null(batch_size)) {
    stopifnot(
      "batch_size must be a positive integer" = is.numeric(batch_size) && batch_size > 0
    )
  }

  api_key <- get_api_key(key_name)

  if (validate) {
    validate_hf_endpoint(endpoint_url, key_name)
  }

  # if batch_size is NULL then we don't batch
  use_batching <- !is.null(batch_size) && nrow(df) > batch_size

  if (use_batching) {
    batch_dfs <- chunk_dataframe(df, batch_size)

    # create our own progress bar for batch processing
    if (progress) {
      cli::cli_progress_bar("Processing batches", total = length(batch_dfs))
    }

    # iterate through batches, update progress bar each completed iteration
    result_list <- list()
    for (i in seq_along(batch_dfs)) {
      batch_df <- batch_dfs[[i]]

      req_df <- hf_build_request_df(
        df = batch_df,
        text_var = !!text_sym,
        endpoint_url = endpoint_url,
        key_name = key_name,
        id_var = !!id_sym,
        max_retries = max_retries,
        timeout = timeout,
        validate = FALSE  # already validated if needed
      )

      #
      if (parallel) {
        resp_df <- hf_perform_parallel(
          df = req_df,
          max_active = max_active,
          progress = FALSE  # using batch progress bar
        )
      } else {
        resp_df <- hf_perform_sequential(
          df = req_df,
          progress = FALSE  # using batch progress bar
        )
      }

      # process completed requests:
      emb_df <- process_chunked_df(
        df = resp_df,
        include_errors = include_errors
      )

      result_list[[i]] <- emb_df

      if (progress) {
        cli::cli_progress_update()
      }
    }

    if (progress) {
      cli::cli_progress_done()
    }

    # combine for returning
    result_df <- dplyr::bind_rows(result_list)

  } else {
    # no batching - prepare all requests at once (is this a bad idea to allow?)
    req_df <- hf_build_request_df(
      df = df,
      text_var = !!text_sym,
      endpoint_url = endpoint_url,
      key_name = key_name,
      id_var = !!id_sym,
      max_retries = max_retries,
      timeout = timeout,
      validate = FALSE
    )

    # either parallel or sequential, depending on user input.
    if (parallel) {
      resp_df <- hf_perform_parallel(
        df = req_df,
        max_active = max_active,
        progress = progress
      )
    } else {
      resp_df <- hf_perform_sequential(
        df = req_df,
        progress = progress
      )
    }

    result_df <- process_chunked_df(
      df = resp_df,
      include_errors = include_errors
    )
  }

  return(result_df)
}
