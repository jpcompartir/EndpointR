#' Process OpenAI embedding API response into a tidy format
#'
#' @description
#' Converts the nested list response from an OpenAI embedding API
#' request into a tidy tibble with embedding vectors as columns.
#'
#' @details
#' This function handles both single document and batch embedding responses.
#' It extracts the embedding vectors and converts them into a wide format
#' tibble where each column (V1, V2, ..., Vn) represents one dimension
#' of the embedding vector. If the response includes index information,
#' it adds an `oai_index` column to preserve the ordering.
#'
#' @param response An httr2 response object or the parsed JSON response
#'   from OpenAI's embedding API
#'
#' @return A tibble containing the embedding vectors as columns (V1, V2, etc.)
#'   and optionally an `oai_index` column if present in the response
#' @export
#'
#' @examples
#' \dontrun{
#'   # Process response from httr2 request
#'   req <- oai_build_embedding_request("Hello world")
#'   resp <- httr2::req_perform(req)
#'   embeddings <- tidy_oai_embedding(resp)
#'
#'   # Process already parsed JSON
#'   resp_json <- httr2::resp_body_json(resp)
#'   embeddings <- tidy_oai_embedding(resp_json)
#' }
tidy_oai_embedding <- function(response) {

  # tries to find the correct data object.

  if (inherits(response, "httr2_response")) {
    resp_json <- httr2::resp_body_json(response)
  } else {
    resp_json <- response
  }

  if (is.list(resp_json) && "data" %in% names(resp_json)) {
    response_data <- resp_json$data
  } else {
    response_data <- resp_json
  }

  # handles the single document case, or a batch of embeddings in a single response.
  rows <- purrr::map(response_data, ~ {

    embedding_values <- unlist(.x$embedding)

    embedding_row <- embedding_values |>
      as.list() |>
      setNames(paste0("V", seq_along(embedding_values))) |>
      tibble::as_tibble()

    if (!is.null(.x$index)) {
      row <- tibble::tibble(oai_index = .x$index) |>
        dplyr::bind_cols(embedding_row)
    } else {
      row <- embedding_row
    }

    row
  })

  result <- purrr::list_rbind(rows)

  return(result)
}


#' Build OpenAI embedding API request
#'
#' @description
#' Creates an httr2 request object configured for OpenAI's embedding API.
#' This is a lower-level function that handles request configuration
#' including authentication, retries, and timeouts.
#'
#' @details
#' This function builds the HTTP request but does not execute it. The request
#' can then be performed using `httr2::req_perform()` or the package's
#' `hf_perform_request()` function.
#'
#' Note that OpenAI has limits on input length - individual inputs cannot
#' exceed the model's token limit (typically 8192 tokens for embedding models).
#' Empty strings are not allowed as input.
#'
#' @param input Character vector of text(s) to embed
#' @param model OpenAI embedding model to use (default: "text-embedding-3-small")
#' @param dimensions Number of embedding dimensions (NULL uses model default)
#' @param max_retries Maximum retry attempts for failed requests (default: 5)
#' @param timeout Request timeout in seconds (default: 20)
#' @param endpoint_url OpenAI API endpoint URL (default: OpenAI's embedding endpoint)
#' @param key_name Name of environment variable containing the API key (default: "OPENAI_API_KEY")
#' @param verbose Whether to enable verbose request logging (default: FALSE)
#'
#' @return An httr2 request object configured for the OpenAI embedding API.
#'   The request object includes a `total_chars` attribute containing the
#'   total character count of the input texts.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Build a simple request
#'   req <- oai_build_embedding_request("Hello world")
#'
#'   # Build request with custom dimensions
#'   req <- oai_build_embedding_request(
#'     input = "Hello world",
#'     dimensions = 512,
#'     model = "text-embedding-3-large"
#'   )
#'
#'   # Perform the request
#'   response <- httr2::req_perform(req)
#' }
oai_build_embedding_request <- function(input, model = "text-embedding-3-small", dimensions = NULL, max_retries = 5, timeout = 20, endpoint_url = "https://api.openai.com/v1/embeddings", key_name = "OPENAI_API_KEY", verbose = FALSE) {

  stopifnot("endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
            "endpoint_url must be a character string" = is.character(endpoint_url),
            "max_retries must be a positive integer" = is.numeric(max_retries) && max_retries > 0,
            "timeout must be a positive number" = is.numeric(timeout) && timeout > 0)

  api_key <- get_api_key(key_name)

  total_chars <- purrr::map_int(input, nchar) |>
    sum()

  request <- base_request(endpoint_url = endpoint_url,
                          api_key = api_key)

  request <- request |>
    httr2::req_body_json(list(
      input = input,
      model = model,
      dimensions = dimensions,
      encoding_format = "float"
      )) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = max_retries,
                     backoff = ~ 2 ^ .x,
                     retry_on_failure = TRUE)

  if (verbose) {
    request <- httr2::req_verbose(req = request,
                                  info = TRUE, header_req = FALSE, header_resp = FALSE, redact_headers = TRUE, body_req = FALSE, body_resp = FALSE)
  }


  attr(request, "total_chars") <- total_chars # so caller func(s) can access this and raise a warning if need be(?) may deprecate

  return(request)

}


# oai_build_embedding_request_batch
oai_build_embedding_request_batch <- function(inputs, model = "text-embedding-3-small", dimensions = NULL, max_retries = 5, timeout = 20, endpoint_url = "https://api.openai.com/v1/embeddings", key_name = "OPENAI_API_KEY") {

  stopifnot("`inputs` must be a list of inputs" = inherits(inputs, "list")|is.vector(inputs),
            "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
            "endpoint_url must be a character string" = is.character(endpoint_url),
            "max_retries must be a positive integer" = is.numeric(max_retries) && max_retries > 0,
            "timeout must be a positive number" = is.numeric(timeout) && timeout > 0)

  api_key <- get_api_key(key_name)

  total_chars <- purrr::map_int(inputs, nchar) |>
    sum()

  base_request <- base_request(
    endpoint_url = endpoint_url,
    api_key = api_key)

  batch_request <- base_request |>
    httr2::req_body_json(
      list(input = inputs, # if this is a list we do have a batch
           model = model,
           dimensions = dimensions,
           encoding_format = "float")
    ) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = max_retries,
                     backoff = ~2 ^ .x,
                     retry_on_failure = TRUE)

  if (verbose) {
    batch_request <- httr2::req_verbose(req = batch_request, info = TRUE)
  }

  attr(batch_request, "total_chars") <- total_chars # so caller func(s) can access this and raise a warning if need be(?) may deprecate

  return(batch_request)

}


#' Generate embeddings for a single text using OpenAI
#'
#' @description
#' High-level function to generate embeddings for a single text string using
#' OpenAI's embedding API. This function handles the entire process from request
#' creation to response processing.
#'
#' @details
#' This function is designed for single text inputs. For processing multiple
#' texts, use `oai_embed_batch()` which is more efficient for batch operations.
#'
#' The function automatically handles API authentication, request retries,
#' and error handling. By default, it returns a tidy tibble with embedding
#' vectors as columns, but you can get the raw response by setting `tidy = FALSE`.
#'
#' @param text Character string to generate embeddings for (must be non-empty)
#' @param model OpenAI embedding model to use (default: "text-embedding-3-small")
#' @param dimensions Number of embedding dimensions (NULL uses model default)
#' @param max_retries Maximum retry attempts for failed requests (default: 5)
#' @param timeout Request timeout in seconds (default: 20)
#' @param endpoint_url OpenAI API endpoint URL (default: OpenAI's embedding endpoint)
#' @param key_name Name of environment variable containing the API key (default: "OPENAI_API_KEY")
#' @param tidy Whether to return a tidy tibble format (default: TRUE)
#'
#' @return If `tidy = TRUE`, returns a tibble with embedding vectors as columns
#'   (V1, V2, etc.). If `tidy = FALSE`, returns the raw httr2 response object.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Generate embeddings for a single text
#'   embeddings <- oai_embed_text("Hello world")
#'
#'   # Use a different model with custom dimensions
#'   embeddings <- oai_embed_text(
#'     text = "Hello world",
#'     model = "text-embedding-3-large",
#'     dimensions = 1024
#'   )
#'
#'   # Get raw response instead of tidy format
#'   raw_response <- oai_embed_text(
#'     text = "Hello world",
#'     tidy = FALSE
#'   )
#' }
oai_embed_text <- function(text,
                           model = "text-embedding-3-small",
                           dimensions = NULL,
                           max_retries = 5,
                           timeout = 20,
                           endpoint_url = "https://api.openai.com/v1/embeddings",
                           key_name = "OPENAI_API_KEY",
                           tidy = TRUE) {

  stopifnot(
    "Text must be a character vector" = is.character(text),
    "Text must not be an empty string" = nchar(text) > 0
  )

  request <- oai_build_embedding_request(
    input = text,
    model = model,
    dimensions = dimensions,
    max_retries = max_retries,
    timeout = timeout,
    endpoint_url = endpoint_url,
    key_name = key_name
  )

  tryCatch({
    response <- hf_perform_request(request)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to generate embeddings",
      "i" = "Text: {cli::cli_vec(text, list('vec-trunc' = 30, 'vec-sep' = ''))}",
      "x" = "Error: {conditionMessage(e)}"
    ))
  })

  # do anything with this info or not? Not for now.
  # status <- response$status_code
  # date <- parse_oai_date(response$headers$date)
  # requests_remaining <- response$headers$`x-ratelimit-remaining-requests`
  # requests_reset_ms <- response$headers$`x-ratelimit-reset-requests`


  if (tidy) {
    response <- tidy_oai_embedding(response)
  }

  return(response)
}


#' Generate embeddings for multiple texts using OpenAI
#'
#' @description
#' High-level function to generate embeddings for multiple text strings using
#' OpenAI's embedding API. This function handles batching, concurrent requests,
#' error handling, and provides progress reporting for large collections of texts.
#'
#' @details
#' This function efficiently processes multiple texts by:
#' 1. Splitting texts into batches of the specified size
#' 2. Creating concurrent requests (if configured) for faster processing
#' 3. Handling individual batch failures gracefully
#' 4. Pre-allocating memory for embeddings to improve performance
#' 5. Providing detailed success/failure reporting
#'
#' If a batch fails, only the documents in that specific batch will be marked
#' as failed, not all documents across all batches. Failed embeddings will be
#' filled with NA values and marked with error information.
#'
#' The function returns a tibble with embedding columns (V1, V2, ..., Vn),
#' error tracking columns (.error, .error_msg), and optionally the
#' original texts.
#'
#' @param texts Vector or list of character strings to generate embeddings for
#' @param model OpenAI embedding model to use (default: "text-embedding-3-small")
#' @param dimensions Number of embedding dimensions (default: 1536 for text-embedding-3-small)
#' @param batch_size Number of texts to process in one API request (default: 10)
#' @param concurrent_requests Number of requests to send simultaneously (default: 1)
#' @param max_retries Maximum retry attempts for failed requests (default: 5)
#' @param timeout Request timeout in seconds (default: 20)
#' @param endpoint_url OpenAI API endpoint URL (default: OpenAI's embedding endpoint)
#' @param key_name Name of environment variable containing the API key (default: "OPENAI_API_KEY")
#' @param include_texts Whether to include original texts in the result (default: TRUE)
#' @param relocate_col Column position to place error columns (default: 2)
#' @param verbose Whether to enable verbose request logging (default: FALSE)
#'
#' @return A tibble containing:
#'   - Embedding vectors as columns (V1, V2, ..., Vn)
#'   - .error: Logical column indicating if embedding failed
#'   - .error_msg: Character column with error details
#'   - text: Original texts (if include_texts = TRUE)
#' @export
#'
#' @examples
#' \dontrun{
#'   # Basic batch embedding
#'   texts <- c("First text", "Second text", "Third text")
#'   embeddings <- oai_embed_batch(texts)
#'
#'   # Large-scale processing with concurrent requests
#'   large_texts <- rep("Sample text", 100)
#'   embeddings <- oai_embed_batch(
#'     texts = large_texts,
#'     batch_size = 20,
#'     concurrent_requests = 5,
#'     dimensions = 512
#'   )
#'
#'   # Custom model and settings
#'   embeddings <- oai_embed_batch(
#'     texts = texts,
#'     model = "text-embedding-3-large",
#'     dimensions = 1024,
#'     include_texts = FALSE,
#'     timeout = 30
#'   )
#' }
oai_embed_batch <- function(texts,
                            model = "text-embedding-3-small",
                            dimensions = 1536,
                            batch_size = 10,
                            concurrent_requests = 1,
                            max_retries = 5,
                            timeout = 20,
                            endpoint_url = "https://api.openai.com/v1/embeddings",
                            key_name = "OPENAI_API_KEY",
                            include_texts = TRUE,
                            relocate_col = 2,
                            verbose = FALSE) {


  if (length(texts) == 0) {
    cli::cli_warn("Input 'texts' is empty. Returning an empty tibble.")
    return(tibble::tibble())
  }

  # 1. Input Validation ----
  stopifnot(
    "Texts must be a list or vector" = is.vector(texts),
    # "Texts must have length > 1" = length(texts) > 1,
    "batch_size must be a positive integer" = is.numeric(batch_size) && batch_size > 0,
    "concurrent_requests must be a positive integer" = is.numeric(concurrent_requests) && concurrent_requests > 0
  )

  n_texts <- length(texts)

  text_classes <- purrr::map(texts, class)
  text_not_empty <- purrr::map_lgl(texts, ~ .x != "")

  stopifnot("Each individual text must be a character vector" = all(text_classes == "character"),
            "Text must not be an empty string" = all(text_not_empty))

  # 2. Creating Batches of Requests ----
  batch_data <- batch_vector(texts, batch_size) # same as hf_*

  batch_requests <- purrr::map(
    batch_data$batch_inputs,
    ~ oai_build_embedding_request( # is fine for batches
      input = .x,
      model = model,
      dimensions = dimensions,
      max_retries = max_retries,
      timeout = timeout,
      endpoint_url = endpoint_url,
      key_name = key_name,
      verbose = verbose
    )
  )


 # 3. Performing Requests ----
  response_list <- perform_requests_with_strategy(
    requests = batch_requests,
    concurrent_requests = concurrent_requests,
    progress = TRUE
  )

  # 4. Tidying Responses:  Matrix -> Data Frame ----
  # pre-allocate vectors for embeddings and errors/messages *and* maintain order
  all_embeddings <- vector("list", n_texts)
  errors <- rep(FALSE, n_texts)
  error_msgs <- rep("", n_texts)

  # now iterate through responses and fill the embeddings and/or errors depending on status
  # works because perform_requests_with_strategy guarantees that we get out responses back in order, despite 'parallelism'
  for (i in seq_along(response_list)) {
    batch_idx <- batch_data$batch_indices[[i]]

    tryCatch({
      resp <- httr2::resp_body_json(response_list[[i]])
      if ("data" %in% names(resp)) {
        for (j in seq_along(resp$data)) {
          if (j <= length(batch_idx)) {
            idx <- batch_idx[j]
            emb <- resp$data[[j]]$embedding
            # make the embedding (list of numerics) a vector of numerics
            all_embeddings[[idx]] <- unlist(emb)
          }
        }
      }
    }, error = function(e) {
      errors[batch_idx] <- TRUE
      error_msgs[batch_idx] <- as.character(e$message)
    })
  }

  mat_list <- purrr::map(all_embeddings, ~ {
    if (is.null(.x) || length(.x) != dimensions) {
      rep(NA_real_, dimensions) # fill with fake vecs if we don't have embeddings
    } else {
      as.numeric(.x) # possibly dangerous coercion to vec? Just means the cells in the DF are values not lists.
    }
  })

  # mem efficient route to emb edding matrix. Then just 1 conversion to tibble/df (not 1 per batch)
  result_matrix <- do.call(rbind, mat_list)
  colnames(result_matrix) <- paste0("V", seq_len(dimensions))

  result <- tibble::as_tibble(result_matrix)


  # 5. Adding Error Information to Data Frame ----
  # add errors and messages to return df. FALSE and "" if no error.
  result$.error <- errors
  result$.error_msg <- error_msgs

  n_failed <- sum(result$.error)
  n_succeeded <- n_texts - n_failed


  if (n_failed > 0) {
    cli::cli_div(theme = list(span.fail = list(color = "red", "font-weight" = "bold"),
                              span.success = list(color = "green")))
    cli::cli_alert_warning("Embedding completed with {.fail {n_failed}} failure{?s}")
    cli::cli_bullets(c(
      "v" = "{.success Successfully embedded: {n_succeeded}} ({round(n_succeeded/n_texts * 100, 1)}%)",
      "x" = "{.fail Failed: {n_failed}} ({round(n_failed/n_texts * 100, 1)}%)"
    ))
    cli::cli_end()
  } else {
    cli::cli_alert_success("{.strong All {n_texts} documents successfully embedded!} {cli::symbol$tick}")
  }

  if (include_texts) {
    result$text <- texts
    result <- dplyr::relocate(result, text, .before = 1)
  }

  # 6. Relocating Cols and Returning ----
  result <- dplyr::relocate(result, c(.error, .error_msg), .before = dplyr::all_of(relocate_col))

  return(result)
}


# oai_embed_chunks docs ----
#' Embed text chunks through OpenAI's Embeddings API
#'
#' This function processes large volumes of text through OpenAI's Embeddings API
#' in configurable chunks, writing results progressively to parquet files. It handles
#' concurrent requests, automatic retries, while managing memory efficiently for
#' large-scale processing.
#'
#' @details This function is designed for processing large text datasets that may not
#' fit comfortably in memory. It divides the input into chunks, processes each chunk
#' with concurrent API requests, and writes results immediately to disk to minimise
#' memory usage.
#'
#' The function preserves data integrity by matching results to source texts through
#' the `ids` parameter. Each chunk is processed independently with results written as
#' parquet files to the output directory.
#'
#' The chunking strategy balances API efficiency with memory management. Larger
#' `chunk_size` values reduce overhead but increase memory usage. Adjust based on
#' your system resources and text sizes.
#'
#' Avoid risk of data loss by setting a low-ish chunk_size (e.g. 5,000, 10,000). Each chunk is written to a `.parquet` file in the `output_dir=` directory, which also contains a `metadata.json` file which tracks important information such as the model and endpoint URL used. Be sure to add output directories to .gitignore!
#'
#' @param texts Character vector of texts to process
#' @param ids Vector of unique identifiers corresponding to each text (same length as texts)
#' @param model OpenAI embedding model to use (default: "text-embedding-3-small")
#' @param dimensions Number of embedding dimensions (default: 1536 for text-embedding-3-small)
#' @param output_dir Path to directory for the .parquet chunks. "auto" generates a timestamped directory name. If NULL, uses a temporary directory.
#' @param chunk_size Number of texts to process in each chunk before writing to disk (default: 5000)
#' @param concurrent_requests Number of concurrent requests (default: 5)
#' @param max_retries Maximum retry attempts per failed request (default: 5)
#' @param timeout Request timeout in seconds (default: 20)
#' @param endpoint_url OpenAI API endpoint URL (default: OpenAI's embedding endpoint)
#' @param key_name Name of environment variable containing the API key (default: "OPENAI_API_KEY")
#' @param id_col_name Name for the ID column in output (default: "id"). When called from oai_embed_df(), this preserves the original column name.
#'
#' @return A tibble with columns:
#'   - ID column (name specified by `id_col_name`): Original identifier from input
#'   - `.error`: Logical indicating if request failed
#'   - `.error_msg`: Error message if failed, NA otherwise
#'   - `.chunk`: Chunk number for tracking
#'   - Embedding columns (V1, V2, etc.)
#' @export
#'
#' @examples
#' \dontrun{
#'   # basic usage with automatic directory naming
#'   result <- oai_embed_chunks(
#'     texts = my_texts,
#'     ids = my_ids,
#'     model = "text-embedding-3-small"
#'   )
#'
#'   # large-scale processing with custom settings
#'   result <- oai_embed_chunks(
#'     texts = my_texts,
#'     ids = my_ids,
#'     output_dir = "my_embeddings",
#'     chunk_size = 10000,
#'     dimensions = 512,
#'     concurrent_requests = 10
#'   )
#' }
# oai_embed_chunks docs ----
oai_embed_chunks <- function(texts,
                             ids,
                             model = "text-embedding-3-small",
                             dimensions = 1536,
                             output_dir = "auto",
                             chunk_size = 5000L,
                             concurrent_requests = 5L,
                             max_retries = 5L,
                             timeout = 20L,
                             endpoint_url = "https://api.openai.com/v1/embeddings",
                             key_name = "OPENAI_API_KEY",
                             id_col_name = "id") {

  # input validation ----
  stopifnot(
    "texts must be a vector" = is.vector(texts),
    "ids must be a vector" = is.vector(ids),
    "texts and ids must be the same length" = length(texts) == length(ids),
    "chunk_size must be a positive integer greater than 1" = is.numeric(chunk_size) && chunk_size > 0
  )

  output_dir <- .handle_output_directory(output_dir, base_dir_name = "oai_embeddings_batch")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  chunk_data <- batch_vector(seq_along(texts), chunk_size)
  n_chunks <- length(chunk_data$batch_indices)

  # write metadata to track important information for debugging and reproducibility
  metadata <- list(
    model = model,
    endpoint_url = endpoint_url,
    dimensions = dimensions,
    chunk_size = chunk_size,
    n_texts = length(texts),
    concurrent_requests = concurrent_requests,
    timeout = timeout,
    max_retries = max_retries,
    output_dir = output_dir,
    key_name = key_name,
    n_chunks = n_chunks,
    timestamp = Sys.time()
  )

  jsonlite::write_json(metadata,
                       file.path(output_dir, "metadata.json"),
                       auto_unbox = TRUE,
                       pretty = TRUE)

  cli::cli_alert_info("Processing {length(texts)} text{?s} in {n_chunks} chunk{?s} of up to {chunk_size} each")
  cli::cli_alert_info("Intermediate results will be saved as parquet files in {output_dir}")

  total_successes <- 0
  total_failures <- 0

  ## chunk processing ----
  for (chunk_num in seq_along(chunk_data$batch_indices)) {

    chunk_indices <- chunk_data$batch_indices[[chunk_num]]
    chunk_texts <- texts[chunk_indices]
    chunk_ids <- ids[chunk_indices]

    cli::cli_progress_message("Processing chunk {chunk_num}/{n_chunks} ({length(chunk_indices)} text{?s})")

    ## build chunk requests ----
    # use individual requests for each text rather than batching within request
    requests <- purrr::map2(
      .x = chunk_texts,
      .y = chunk_ids,
      .f = function(text, id) {
        req <- oai_build_embedding_request(
          input = text,
          model = model,
          dimensions = dimensions,
          max_retries = max_retries,
          timeout = timeout,
          endpoint_url = endpoint_url,
          key_name = key_name
        )
        # attach id to request headers for tracking
        httr2::req_headers(req, endpointr_id = id)
      }
    )

    # make sure we have some valid requests, or skip to the next iteration
    is_valid_request <- purrr::map_lgl(requests, ~inherits(.x, "httr2_request"))
    valid_requests <- requests[is_valid_request]

    if (length(valid_requests) == 0) {
      cli::cli_alert_warning("No valid request{?s} in chunk {chunk_num}, skipping")
      next
    }

    # perform chunk requests ----
    responses <- perform_requests_with_strategy(
      valid_requests,
      concurrent_requests = concurrent_requests,
      progress = TRUE
    )

    successes <- httr2::resps_successes(responses)
    failures <- httr2::resps_failures(responses)

    n_successes <- length(successes)
    n_failures <- length(failures)
    total_successes <- total_successes + n_successes
    total_failures <- total_failures + n_failures

    ## process chunk responses ----
    # within chunk results
    chunk_results <- list()

    if (n_successes > 0) {
      successes_ids <- purrr::map(successes, ~purrr::pluck(.x, "request", "headers", "endpointr_id")) |> unlist()
      successes_content <- purrr::map(successes, tidy_oai_embedding) |>
        purrr::list_rbind()

      chunk_results$successes <- tibble::tibble(
        !!id_col_name := successes_ids,
        .error = FALSE,
        .error_msg = NA_character_,
        .chunk = chunk_num
      ) |>
        dplyr::bind_cols(successes_content)
    }

    if (n_failures > 0) {
      failures_ids <- purrr::map(failures, ~purrr::pluck(.x, "request", "headers", "endpointr_id")) |> unlist()
      failures_msgs <- purrr::map_chr(failures, ~purrr::pluck(.x, "message", .default = "Unknown error"))

      chunk_results$failures <- tibble::tibble(
        !!id_col_name := failures_ids,
        .error = TRUE,
        .error_msg = failures_msgs,
        .chunk = chunk_num
      )
    }

    chunk_df <- dplyr::bind_rows(chunk_results)

    if (nrow(chunk_df) > 0) {
      chunk_file <- glue::glue("{output_dir}/chunk_{stringr::str_pad(chunk_num, 3, pad = '0')}.parquet")
      arrow::write_parquet(chunk_df, chunk_file)
    }

    cli::cli_alert_success("Chunk {chunk_num}: {n_successes} successful, {n_failures} failed")

    rm(requests, responses, successes, failures, chunk_results, chunk_df)
    gc(verbose = FALSE)
  }

  parquet_files <- list.files(output_dir, pattern = "\\.parquet$", full.names = TRUE)

  cli::cli_alert_info("Processing completed, there were {total_successes} successes\n and {total_failures} failures.")
  final_results <- arrow::open_dataset(parquet_files, format = "parquet") |>
    dplyr::collect()

  return(final_results)
}



#' Generate embeddings for texts in a data frame using OpenAI
#'
#' @description
#' High-level function to generate embeddings for texts in a data frame using
#' OpenAI's embedding API. This function handles the entire process from request
#' creation to response processing, with options for chunking & concurrent requests.
#'
#' @details
#' This function extracts texts from a specified column, generates embeddings using
#' `oai_embed_chunks()`, and returns the results matched to the original IDs.
#'
#' The chunking approach enables processing of large data frames without memory
#' constraints. Results are written progressively as parquet files (either to a specified
#' directory or auto-generated) and then read back as the return value.
#'
#' OpenAI's embedding API allows you to specify the number of dimensions for the
#' output embeddings, which can be useful for reducing memory usage, storage costs, or matching
#' specific downstream requirements. The default is model-specific (1536 for
#' text-embedding-3-small). \href{https://openai.com/index/new-embedding-models-and-api-updates/}{OpenAI Embedding Updates}
#'
#' Avoid risk of data loss by setting a low-ish chunk_size (e.g. 5,000, 10,000). Each chunk is written to a `.parquet` file in the `output_dir=` directory, which also contains a `metadata.json` file. Be sure to add output directories to .gitignore!
#'
#' @param df Data frame containing texts to embed
#' @param text_var Column name (unquoted) containing texts to embed
#' @param id_var Column name (unquoted) for unique row identifiers
#' @param model OpenAI embedding model to use (default: "text-embedding-3-small")
#' @param dimensions Number of embedding dimensions (default: 1536)
#' @param key_name Name of environment variable containing the API key
#' @param output_dir Path to directory for the .parquet chunks. "auto" generates a timestamped directory name. If NULL, uses a temporary directory.
#' @param chunk_size Number of texts to process in each chunk before writing to disk (default: 5000)
#' @param concurrent_requests Number of concurrent requests (default: 1)
#' @param max_retries Maximum retry attempts per request (default: 5)
#' @param timeout Request timeout in seconds (default: 20)
#' @param endpoint_url OpenAI API endpoint URL
#' @param progress Whether to display a progress bar (default: TRUE)
#'
#' @return A tibble with columns:
#'   - ID column (preserves original column name): Original identifier from input
#'   - `.error`: Logical indicating if request failed
#'   - `.error_msg`: Error message if failed, NA otherwise
#'   - `.chunk`: Chunk number for tracking
#'   - Embedding columns (V1, V2, etc.)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(
#'     id = 1:3,
#'     text = c("First example", "Second example", "Third example")
#'   )
#'
#'   # Generate embeddings with default settings
#'   embeddings_df <- oai_embed_df(
#'     df = df,
#'     text_var = text,
#'     id_var = id
#'   )
#'
#'   # Generate embeddings with custom dimensions
#'   embeddings_df <- oai_embed_df(
#'     df = df,
#'     text_var = text,
#'     id_var = id,
#'     dimensions = 360,  # smaller embeddings
#'     batch_size = 5
#'   )
#'
#'   # Use with concurrent requests for faster processing
#'   embeddings_df <- oai_embed_df(
#'     df = df,
#'     text_var = text,
#'     id_var = id,
#'     model = "text-embedding-3-large",
#'     concurrent_requests = 3
#'   )
#' }
oai_embed_df <- function(df,
                         text_var,
                         id_var,
                         model = "text-embedding-3-small",
                         dimensions = 1536,
                         key_name = "OPENAI_API_KEY",
                         batch_size = 10,
                         concurrent_requests = 1,
                         max_retries = 5,
                         timeout = 20,
                         endpoint_url = "https://api.openai.com/v1/embeddings",
                         progress = TRUE) {

  text_sym <- rlang::ensym(text_var)
  id_sym <- rlang::ensym(id_var)

  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
    "concurrent_requests must be a number greater than 0" = is.numeric(concurrent_requests) && concurrent_requests > 0,
    "batch_size must be a number greater than 0" = is.numeric(batch_size) && batch_size > 0
  )

  if (!rlang::as_string(text_sym) %in% names(df)) {
    cli::cli_abort("Column {.code {rlang::as_string(text_sym)}} not found in data frame")
  }

  if (!rlang::as_string(id_sym) %in% names(df)) {
    cli::cli_abort("Column {.code {rlang::as_string(id_sym)}} not found in data frame")
  }

  original_num_rows <- nrow(df)

  # pull texts & ids into vectors for batch function
  texts <- dplyr::pull(df, !!text_sym)
  indices <- dplyr::pull(df, !!id_sym)

  batch_size <- if(is.null(batch_size) || batch_size <= 1) 1 else batch_size

  embeddings_tbl <- oai_embed_batch(
    texts = texts,
    model = model,
    dimensions = dimensions,
    batch_size = batch_size,
    concurrent_requests = concurrent_requests,
    max_retries = max_retries,
    timeout = timeout,
    endpoint_url = endpoint_url,
    key_name = key_name,
    include_texts = FALSE,
    relocate_col = 1
  )

  df_with_row_id <- df |> dplyr::mutate(.row_id = dplyr::row_number())

  embeddings_tbl <- embeddings_tbl |>
    dplyr::mutate(.row_id = dplyr::row_number())

  result_df <- df_with_row_id |>
    dplyr::left_join(embeddings_tbl, by = ".row_id") |>
    dplyr::select(-.row_id)

  # sanity check and alert user if there's a mismatch
  final_num_rows <- nrow(result_df)

  if(final_num_rows != original_num_rows){
    cli::cli_warn("Rows in original data frame and returned data frame do not match:")
    cli::cli_bullets(text = c(
      "Rows in original data frame: {original_num_rows}",
      "Rows in returned data frame: {final_num_rows}"
    ))
  }

  return(result_df)
}
