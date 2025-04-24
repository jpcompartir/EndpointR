# hf_embed_text docs ----
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
#' @param ... ellipsis sent to `hf_perform_request`, which forwards to `httr2::req_perform`
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
# hf_embed_text docs ----
hf_embed_text <- function(text,
                         endpoint_url,
                         key_name,
                         ...,
                         max_retries = 3,
                         timeout = 10,
                         validate = FALSE) {

  # build request with the specified parameters
  req <- hf_build_request(
    input = text,
    endpoint_url = endpoint_url,
    key_name = key_name,
    max_retries = max_retries,
    timeout = timeout,
    validate = validate
  )

  # perform request and provide user-friendly error messages
  tryCatch({
    embeddings <- hf_perform_request(req, ..., tidy = TRUE)
    return(embeddings)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to generate embeddings",
      "i" = "Text: {cli::cli_vec(text, list('vec-trunc' = 30, 'vec-sep' = ''))}",
      "x" = "Error: {conditionMessage(e)}"
    ))
  })
}


# hf_embed_batch docs ----
#' Generate batches of embeddings for a list of texts
#'
#' @description
#' High-level function to generate embeddings for multiple text strings.
#' This function handles batching and parallel processing of embedding requests.
#'
#' @param texts Vector or list of character strings to get embeddings for
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#' @param ... ellipsis sent to `hf_perform_request` TODO (reserved ATM)
#' @param batch_size Number of texts to process in one batch
#' @param concurrent_requests Number of requests to send simultaneously
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param timeout Request timeout in seconds
#' @param validate Whether to validate the endpoint before creating the request
#'
#' @return A tibble containing the embedding vectors
#' @export
#'
#' @examples
#' \dontrun{
#'   # Generate embeddings for multiple texts using default batch size
#'   embeddings <- hf_embed_batch(
#'     texts = c("First example", "Second example", "Third example"),
#'     endpoint_url = "https://my-endpoint.huggingface.cloud"
#'   )
#'
#'   # With custom batch size and concurrent requests
#'   embeddings <- hf_embed_batch(
#'     texts = c("First example", "Second example", "Third example"),
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     batch_size = 10,
#'     concurrent_requests = 2
#'   )
#' }
# hf_embed_batch docs ----
hf_embed_batch <- function(texts, endpoint_url, key_name, ..., batch_size = 8,
                           concurrent_requests = 1, max_retries = 5, timeout = 10, validate = FALSE){
  stopifnot(
    "Texts must be a list or vector" = is.vector(texts),
    "batch_size must be a numeric input" = is.numeric(batch_size)
  )


  # batch if we need to, don't if we don't (conditional 1 starts)
  if(length(texts) > batch_size){

    batch_indices <- split(seq_along(texts), ceiling(seq_along(texts) / batch_size)) # returns list of:
    # pos 1:batch_size get ceiling'd value of 1
    # pos batch_size:2(batch_size) get ceiling'd value of 2, and so-on

    batch_texts <- purrr::map(batch_indices, ~texts[.x])

    batch_reqs <- purrr::map(batch_texts, ~hf_build_request_batch(.x, endpoint_url, key_name,
                                                                  max_retries = max_retries,
                                                                  timeout = timeout,
                                                                  validate = validate))


    if (concurrent_requests > 1) {  # (conditional 2 starts)
      # concurrent_requests at a time
      batch_resps <- httr2::req_perform_parallel(batch_reqs,
                                                 on_error = "continue",
                                                 progress = TRUE,
                                                 max_active = concurrent_requests)

      result_list <- purrr::map(batch_resps, ~tidy_embedding_response(.x))

    } else { # (conditional 2 ends)
      #  one request at a time
      result_list <- purrr::map(batch_reqs, ~hf_perform_request(.x, tidy = FALSE) |>
                                  tidy_embedding_response())
    }

    result <- purrr::list_rbind(result_list) # purrr-friendly bind_rows()

  } else { # (conditional 1 ends)
    # we didn't need to batch, so just prepare and send one request
    batch_req <- hf_build_request_batch(texts, endpoint_url, key_name,
                                        max_retries = max_retries,
                                        timeout = timeout,
                                        validate = validate)

    resp <- hf_perform_request(batch_req, tidy = FALSE)
    result <- tidy_embedding_response(resp)
  }

  return(result)
}

# tidy_embedding_response_docs ----
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
#'   req <- hf_build_request(text, endpoint_url, api_key)
#'   resp <- httr2::req_perform(req)
#'   embeddings <- tidy_embedding_response(resp)
#'
#'   # Process already parsed JSON
#'   resp_json <- httr2::resp_body_json(resp)
#'   embeddings <- tidy_embedding_response(resp_json)
#' }
# tidy_embedding_response_docs ----
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

# tidy_chunked_embedding_df docs ----
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
#'   embeddings_df <- tidy_chunked_embedding_df(
#'     df = responses_df,
#'     include_errors = FALSE
#'   )
#' }
# tidy_chunked_embedding_df docs ----
tidy_chunked_embedding_df <- function(df, include_errors = FALSE) {
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

# hf_embed_df docs ----
#' Generate embeddings for texts in a data frame
#'
#' @description
#' High-level function to generate embeddings for texts in a data frame.
#' This function handles the entire process from request creation to
#' response processing, with options for batching & parallel execution.
#' Setting the number of retries,
#'
#' @param df A data frame containing texts to embed
#' @param text_var Name of the column containing text to embed
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param id_var Name of the column to use as ID
#' @param key_name Name of the environment variable containing the API key
#' @param batch_size Number of texts to process in one batch (NULL for no batching)
#' @param concurrent_requests Number of requests to send at once. Some APIs do not allow for multiple requests.
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
# hf_embed_df docs ----
hf_embed_df <- function(df,
                       text_var,
                       endpoint_url,
                       key_name,
                       id_var,
                       batch_size = NULL,
                       concurrent_requests = 1,
                       max_retries = 3,
                       timeout = 10,
                       progress = TRUE,
                       validate = FALSE,
                       include_errors = TRUE) {


  text_sym <- rlang::ensym(text_var)
  id_sym <- rlang::ensym(id_var)

  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
    "concurrent_requests must be an integer" = is.numeric(concurrent_requests)
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
      if (concurrent_requests > 1) {
        resp_df <- hf_perform_parallel_df(
          df = req_df,
          max_active = concurrent_requests,
          progress = FALSE  # using batch progress bar
        )
      } else {
        resp_df <- hf_perform_sequential_df(
          df = req_df,
          progress = FALSE  # using batch progress bar
        )
      }

      # process completed requests:
      emb_df <- tidy_chunked_embedding_df(
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
    if (concurrent_requests > 1) {
      resp_df <- hf_perform_parallel_df(
        df = req_df,
        max_active = concurrent_requests,
        progress = progress
      )
    } else {
      resp_df <- hf_perform_sequential_df(
        df = req_df,
        progress = progress
      )
    }

    result_df <- tidy_chunked_embedding_df(
      df = resp_df,
      include_errors = include_errors
    )
  }

  return(result_df)
}
