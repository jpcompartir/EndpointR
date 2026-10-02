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
  if (inherits(response, "httr2_response")) {
    resp_json <- httr2::resp_body_json(response)
  } else {
    resp_json <- response
  }

  if (is.list(resp_json) && !is.null(names(resp_json))) {
    if ("embedding" %in% names(resp_json)) {
      resp_json <- list(resp_json$embedding)
    }
  }

  tib <- sapply(resp_json, unlist) |>
    t() |> # transpose to wide form
    as.data.frame.matrix() |>
    tibble::as_tibble()

  return(tib)
}


# hf_embed_text docs ----
#' Generate embeddings for a single text
#'
#' @description
#' High-level function to generate embeddings for a single text string.
#' This function handles the entire process from request creation to
#' response processing.
#'
#' @details
#' The text is sent as a batch of one, in the request format for the
#' endpoint's inference engine (see the `engine` argument).
#'
#' @param text Character string to get embeddings for
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#' @param ... ellipsis sent to `hf_perform_request`, which forwards to `httr2::req_perform`
#' @param parameters Advanced usage: parameters to pass to the API endpoint. On
#'   TEI endpoints these are added to the top level of the request body.
#' @param tidy Whether to attempt to tidy the response or not
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param timeout Request timeout in seconds
#' @param validate Whether to validate the endpoint before creating the request
#' @param engine The endpoint's inference engine: `"auto"` (default) detects it
#'   with a call to the endpoint's `/info` route, `"tei"` for Text Embeddings
#'   Inference, `"toolkit"` for the default Hugging Face Inference Toolkit.
#'   Set the default for a session with `options(EndpointR.hf_engine = "tei")`.
#'
#' @return A tibble containing the embedding vectors
#' @export
#'
#' @examples
#' \dontrun{
#'   # Generate embeddings using API key from environment
#'   embeddings <- hf_embed_text(
#'     text = "This is a sample text to embed",
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     key_name = "HF_API_KEY"
#'   )
#' }
# hf_embed_text docs ----
hf_embed_text <- function(text,
                         endpoint_url,
                         key_name,
                         ...,
                         parameters = list(),
                         tidy = TRUE,
                         max_retries = 5,
                         timeout = 120,
                         validate = FALSE,
                         engine = getOption("EndpointR.hf_engine", "auto")) {

  stopifnot(
    "Text must be a character vector" = is.character(text)
  )

  if (validate) {
    validate_hf_endpoint(endpoint_url, key_name)
  }

  engine <- .hf_resolve_engine(engine, endpoint_url, key_name)

  req <- hf_build_request_batch(inputs = text,
                                parameters = parameters,
                                endpoint_url = endpoint_url,
                                key_name = key_name,
                                max_retries = max_retries,
                                timeout = timeout,
                                engine = engine$engine,
                                task = "embed")

  # provide user-friendly error messages
  tryCatch({
    response <- hf_perform_request(req, ...)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to generate embeddings",
      "i" = "Text: {cli::cli_vec(text, list('vec-trunc' = 30, 'vec-sep' = ''))}",
      "x" = "Error: {conditionMessage(e)}"
    ))
  })

  if (tidy) {
    response <- tidy_embedding_response(response)
  }

  return(response)
}


# hf_embed_batch docs ----
#' Generate batches of embeddings for a list of texts
#'
#' @description
#' High-level function to generate embeddings for multiple text strings.
#' This function sends several texts per request and several requests at once,
#' and attempts to handle errors gracefully.
#'
#' @details
#' Texts are sent in batches of `batch_size`, with `concurrent_requests`
#' requests in flight. When a batch fails with a client error (400, 413, 422
#' or 424) or a network error, it is split in half and sent again, down to
#' single texts, so only the text at fault fails. Requests that get 429 or 5xx
#' are re-sent unchanged, up to `max_retries` times.
#'
#' Empty and missing texts are not sent. They are returned as error rows.
#'
#' TEI endpoints reject requests with more texts than their
#' `max_client_batch_size` (32 by default). When `batch_size` is larger,
#' EndpointR lowers it with a warning.
#'
#' @param texts Vector or list of character strings to get embeddings for
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#' @param ... Reserved for future use
#' @param tidy_func Function to process/tidy the raw API response (default: tidy_embedding_response)
#' @param parameters Advanced usage: parameters to pass to the API endpoint. On
#'   TEI endpoints these are added to the top level of the request body.
#' @param batch_size Number of texts to send in each request (default: 32)
#' @param include_texts Whether to return the original texts in the return tibble
#' @param concurrent_requests Number of requests to send simultaneously (default: 16)
#' @param max_retries Maximum number of re-sends for requests that get 429 or 5xx
#' @param timeout Request timeout in seconds
#' @param validate Whether to validate the endpoint before creating the request
#' @param relocate_col Which position in the data frame to relocate the results to.
#' @param engine The endpoint's inference engine: `"auto"` (default), `"tei"`
#'   or `"toolkit"`. See [hf_embed_text()].
#' @param progress Whether to show a progress bar
#'
#' @return A tibble containing the embedding vectors
#' @export
#'
#' @examples
#' \dontrun{
#'   embeddings <- hf_embed_batch(
#'     texts = c("First example", "Second example", "Third example"),
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     key_name = "HF_API_KEY",
#'     batch_size = 32,
#'     concurrent_requests = 16
#'   )
#' }
# hf_embed_batch docs ----
hf_embed_batch <- function(texts,
                           endpoint_url,
                           key_name,
                           ...,
                           tidy_func = tidy_embedding_response,
                           parameters = list(),
                           batch_size = 32,
                           include_texts = TRUE,
                           concurrent_requests = 16,
                           max_retries = 5,
                           timeout = 120,
                           validate = FALSE,
                           relocate_col = 2,
                           engine = getOption("EndpointR.hf_engine", "auto"),
                           progress = TRUE) {

  # input validation ----
  if (length(texts) == 0) {
    cli::cli_warn("Input 'texts' is empty. Returning an empty tibble.")
    return(tibble::tibble())
  }

  stopifnot(
    "Texts must be a list or vector" = is.vector(texts),
    "batch_size must be a positive integer" = is.numeric(batch_size) && batch_size > 0 && batch_size == as.integer(batch_size),
    "concurrent_requests must be a positive integer" = is.numeric(concurrent_requests) && concurrent_requests > 0 && concurrent_requests == as.integer(concurrent_requests),
    "max_retries must be a positive integer" = is.numeric(max_retries) && max_retries >= 0 && max_retries == as.integer(max_retries),
    "timeout must be a positive integer" = is.numeric(timeout) && timeout > 0,
    "endpoint_url must be a non-empty string" = is.character(endpoint_url) && nchar(endpoint_url) > 0,
    "key_name must be a non-empty string" = is.character(key_name) && nchar(key_name) > 0
  )

  texts <- unlist(texts)
  api_key <- get_api_key(key_name)

  if (validate) {
    validate_hf_endpoint(endpoint_url, key_name)
  }

  engine <- .hf_resolve_engine(engine, endpoint_url, key_name)
  batch_size <- .hf_check_batch_size(batch_size, engine)

  processed <- .hf_process_texts(
    texts = texts,
    endpoint_url = endpoint_url,
    api_key = api_key,
    engine = engine$engine,
    task = "embed",
    tidy_func = tidy_func,
    batch_size = batch_size,
    concurrent_requests = concurrent_requests,
    max_retries = max_retries,
    timeout = timeout,
    parameters = parameters,
    progress = progress
  )

  # formatting results ----
  result <- processed$results

  if (include_texts) {
    result$text <- texts[result$.row]
    result <- result |> dplyr::relocate(text, .before = 1)
  }

  result$.row <- NULL

  result <- dplyr::relocate(result, c(`.error`, `.error_msg`), .before = dplyr::all_of(relocate_col))
  return(result)
}


# hf_embed_chunks docs ----
#' Embed text chunks through Hugging Face Inference Embedding Endpoints
#'
#' This function is capable of processing large volumes of text through Hugging Face's Inference Embedding Endpoints. Results are written in chunks to a file, to avoid out of memory issues.
#'
#' @details This function processes texts in chunks. Within each chunk, texts
#' are sent in batches of `batch_size` texts per request, with
#' `concurrent_requests` requests in flight. After each chunk, its results are
#' written to a `.parquet` file in `output_dir`.
#'
#' When a batch fails with a client error (400, 413, 422 or 424) or a network
#' error, it is split in half and sent again, down to single texts, so only the
#' text at fault fails. Empty and missing texts are not sent; they are returned
#' as error rows. Results are returned in input order.
#'
#' The engine, batch size, endpoint limits (on TEI), number of empty texts and
#' number of split batches are recorded in `metadata.json`.
#'
#' @param texts Character vector of texts to process
#' @param ids Vector of unique identifiers corresponding to each text (same length as texts)
#' @param endpoint_url Hugging Face Embedding Endpoint
#' @param output_dir Path to directory for the .parquet chunks
#' @param overwrite If `FALSE` (default), errors when `output_dir` already contains chunk (`.parquet`) or `metadata.json` files. Set to `TRUE` to delete them and write fresh outputs; other files are left untouched.
#' @param chunk_size Number of texts to process in each chunk before writing to disk (default: 5000)
#' @param batch_size Number of texts to send in each request (default: 32)
#' @param concurrent_requests Number of concurrent requests (default: 16)
#' @param max_retries Maximum re-sends for requests that get 429 or 5xx (default: 5)
#' @param timeout Request timeout in seconds (default: 120)
#' @param key_name Name of environment variable containing the API key (default: "HF_API_KEY")
#' @param id_col_name Name for the ID column in output (default: "id"). When called from hf_embed_df(), this preserves the original column name.
#' @param engine The endpoint's inference engine: `"auto"` (default), `"tei"`
#'   or `"toolkit"`. See [hf_embed_text()].
#' @param progress Whether to show a progress bar
#'
#' @return A tibble with columns:
#'   - ID column (name specified by `id_col_name`): Original identifier from input
#'   - `.error`: Logical indicating if request failed
#'   - `.error_msg`: Error message if failed, NA otherwise
#'   - `.status`: HTTP status code of a failed request, NA otherwise
#'   - `.chunk`: Chunk number for tracking
#'   - Embedding columns (V1, V2, etc.)
#' @export
#'
# hf_embed_chunks docs ----
hf_embed_chunks <- function(texts,
                            ids,
                            endpoint_url,
                            output_dir = "auto",
                            overwrite = FALSE,
                            chunk_size = 5000L,
                            batch_size = 32L,
                            concurrent_requests = 16L,
                            max_retries = 5L,
                            timeout = 120L,
                            key_name = "HF_API_KEY",
                            id_col_name = "id",
                            engine = getOption("EndpointR.hf_engine", "auto"),
                            progress = TRUE) {

  # input validation ----
  stopifnot(
    "texts must be a vector" = is.vector(texts),
    "ids must be a vector" = is.vector(ids),
    "texts and ids must be the same length" = length(texts) == length(ids),
    "chunk_size must be a positive integer greater than 1" = is.numeric(chunk_size) && chunk_size > 0,
    "batch_size must be a positive integer" = is.numeric(batch_size) && batch_size > 0 && batch_size == as.integer(batch_size),
    "concurrent_requests must be a positive integer" = is.numeric(concurrent_requests) && concurrent_requests > 0
  )

  output_dir <- .handle_output_directory(output_dir, base_dir_name = "hf_embeddings_batch")
  .check_existing_output(output_dir, overwrite = overwrite)

  api_key <- get_api_key(key_name)
  engine <- .hf_resolve_engine(engine, endpoint_url, key_name)
  batch_size <- .hf_check_batch_size(batch_size, engine)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  chunk_data <- batch_vector(seq_along(texts), chunk_size)
  n_chunks <- length(chunk_data$batch_indices)

  # write/store important metadata in the output dir
  metadata <- c(
    list(
      endpoint_url = endpoint_url,
      chunk_size = chunk_size,
      batch_size = batch_size,
      n_texts = length(texts),
      concurrent_requests = concurrent_requests,
      timeout = timeout,
      max_retries = max_retries,
      output_dir = output_dir,
      key_name = key_name,
      n_chunks = n_chunks,
      timestamp = Sys.time()
    ),
    .hf_engine_metadata(engine)
  )
  .hf_write_metadata(metadata, output_dir)

  cli::cli_alert_info("Processing {length(texts)} text{?s} in {n_chunks} chunk{?s} of up to {chunk_size} each, {batch_size} text{?s} per request")
  cli::cli_alert_info("Intermediate results will be saved as parquet files in {output_dir}")

  total_success <- 0
  total_failures <- 0
  total_empty <- 0
  total_splits <- 0

  ## Chunk Processing ----
  for (chunk_num in seq_along(chunk_data$batch_indices)) {

    chunk_indices <- chunk_data$batch_indices[[chunk_num]]

    cli::cli_progress_message("Processing chunk {chunk_num}/{n_chunks} ({length(chunk_indices)} text{?s})")

    processed <- .hf_process_texts(
      texts = texts[chunk_indices],
      endpoint_url = endpoint_url,
      api_key = api_key,
      engine = engine$engine,
      task = "embed",
      tidy_func = tidy_embedding_response,
      batch_size = batch_size,
      concurrent_requests = concurrent_requests,
      max_retries = max_retries,
      timeout = timeout,
      progress = progress
    )

    chunk_df <- processed$results
    chunk_df$.chunk <- chunk_num
    chunk_df <- chunk_df |>
      dplyr::mutate(!!id_col_name := ids[chunk_indices][.data$.row], .before = 1) |>
      dplyr::relocate(".chunk", .after = ".status")
    chunk_df$.row <- NULL

    n_failures <- sum(chunk_df$.error)
    n_successes <- nrow(chunk_df) - n_failures
    total_success <- total_success + n_successes
    total_failures <- total_failures + n_failures
    total_empty <- total_empty + processed$n_empty
    total_splits <- total_splits + processed$n_splits

    .hf_write_chunk(chunk_df, output_dir, chunk_num)

    cli::cli_alert_success("Chunk {chunk_num}: {n_successes} successful, {n_failures} failed")
  }

  metadata$n_empty_texts <- total_empty
  metadata$n_split_batches <- total_splits
  .hf_write_metadata(metadata, output_dir)

  cli::cli_alert_info("Processing completed, there were {total_success} successes\n and {total_failures} failures.")

  .hf_read_chunks(output_dir)
}


# hf_embed_df docs ----
#' Generate embeddings for texts in a data frame
#'
#' @description
#' High-level function to generate embeddings for texts in a data frame.
#' This function handles the entire process from request creation to
#' response processing, with options for batching & parallel execution.
#'
#' Avoid risk of data loss by setting a low-ish chunk_size (e.g. 5,000, 10,000). Each chunk is written to a `.parquet` file in the `output_dir=` directory, which also contains a `metadata.json` file which tracks important information such as the endpoint URL used. Be sure to check any output directories into .gitignore!
#'
#' @details
#' See [hf_embed_chunks()] for how texts are batched, retried and split.
#'
#' @param df A data frame containing texts to embed
#' @param text_var Name of the column containing text to embed
#' @param id_var Name of the column to use as ID
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#' @param output_dir Path to directory for the .parquet chunks
#' @param overwrite If `FALSE` (default), errors when `output_dir` already contains chunk (`.parquet`) or `metadata.json` files. Set to `TRUE` to delete them and write fresh outputs; other files are left untouched.
#' @param chunk_size The size of each chunk that will be processed and then written to a file.
#' @param batch_size Number of texts to send in each request (default: 32)
#' @param concurrent_requests Number of requests to send at once (default: 16)
#' @param max_retries Maximum re-sends for requests that get 429 or 5xx.
#' @param timeout Request timeout in seconds
#' @param progress Whether to display a progress bar
#' @param engine The endpoint's inference engine: `"auto"` (default), `"tei"`
#'   or `"toolkit"`. See [hf_embed_text()].
#'
#' @return A data frame with the original data plus embedding columns
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(
#'     id = 1:3,
#'     text = c("First example", "Second example", "Third example")
#'   )
#'
#'   embeddings_df <- hf_embed_df(
#'     df = df,
#'     text_var = text,
#'     id_var = id,
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     key_name = "HF_API_KEY",
#'     batch_size = 32,
#'     concurrent_requests = 16
#'   )
#' }
# hf_embed_df docs ----
hf_embed_df <- function(df,
                        text_var,
                        id_var,
                        endpoint_url,
                        key_name,
                        output_dir = "auto",
                        overwrite = FALSE,
                        chunk_size = 5000L,
                        batch_size = 32L,
                        concurrent_requests = 16L,
                        max_retries = 5L,
                        timeout = 120L,
                        progress = TRUE,
                        engine = getOption("EndpointR.hf_engine", "auto")) {

  text_sym <- rlang::ensym(text_var)
  id_sym <- rlang::ensym(id_var)

  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "df must not be empty" = nrow(df) > 0,
    "text_var must exist in df" = rlang::as_name(text_sym) %in% names(df),
    "id_var must exist in df" = rlang::as_name(id_sym) %in% names(df),
    "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
    "concurrent_requests must be an integer" = is.numeric(concurrent_requests) && concurrent_requests > 0
  )

  output_dir <- .handle_output_directory(output_dir,
                                         base_dir_name = "hf_embeddings_batch")

  texts <- dplyr::pull(df, !!text_sym)
  indices <- dplyr::pull(df, !!id_sym)

  # preserve original column name
  id_col_name <- rlang::as_name(id_sym)

  chunk_size <- if (is.null(chunk_size) || chunk_size <= 1) 1 else chunk_size

  results <- hf_embed_chunks(
    texts = texts,
    ids = indices,
    endpoint_url = endpoint_url,
    key_name = key_name,
    chunk_size = chunk_size,
    batch_size = batch_size,
    concurrent_requests = concurrent_requests,
    max_retries = max_retries,
    timeout = timeout,
    output_dir = output_dir,
    overwrite = overwrite,
    id_col_name = id_col_name,
    engine = engine,
    progress = progress
  )

  return(results)
}
