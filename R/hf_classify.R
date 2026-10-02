# space for classifying text data with HF Inference Endpoints
# functions from core and hf_inference will be helpful to re-use
# functions from hf_embed serve as :sparkles: inspo :sparkles:

# tidy_classification_response_docs ----
#' Convert Hugging Face classification response to tidy format
#'
#' @description
#' Transforms the nested JSON response from a Hugging Face classification
#' endpoint into a tidy data frame with one row and columns for each
#' classification label.
#'
#' @details
#' This function expects a specific structure in the response, with
#' each classification result containing a 'label' and 'score' field.
#' It flattens the nested structure and pivots the data to create a
#' wide-format data frame.
#'
#' The function accepts either a raw `httr2_response` object or a parsed
#' JSON structure, making it flexible for different workflow patterns.
#'
#' @param response Either an httr2_response object from a Hugging Face API
#'   request or a parsed JSON object containing classification results
#'
#' @return A data frame with one row and columns for each classification label
#' @export
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#'   # Process response directly from API call
#'   response <- hf_perform_request(req)
#'   tidy_results <- tidy_classification_response(response)
#'
#'   # Or with an already-parsed JSON object
#'   json_data <- httr2::resp_body_json(response)
#'   tidy_results <- tidy_classification_response(json_data)
#'
#'   # Example of expected output structure
#'   # A tibble: 1 × 2
#'   #   positive negative
#'   #      <dbl>    <dbl>
#'   # 1    0.982    0.018
#' }
# tidy_classification_response_docs ----
tidy_classification_response <- function(response){

  if (inherits(response, "httr2_response")) {
    resp_json <- httr2::resp_body_json(response)
  } else {
    resp_json <- response
  }

  # sort later, we're basically going to unlist and pivot?
  # might be better for users to build these themselves and we handle
  # peaks and pits, sentiment, or something

  tidy_response <-
    purrr::flatten(resp_json) |>
      purrr::map(~ data.frame(label = .x$label,
                            score = .x$score)) |>  # will need to wrap this in a tryCatch.
      purrr::list_rbind() |>
      tidyr::pivot_wider(names_from = label, values_from = score)


  return(tidy_response)
}


# need a separate func for batch classifications, as we planned with embeddings.
tidy_batch_classification_response <- function(response) {
  if (inherits(response, "httr2_response")) {
    resp_json <- httr2::resp_body_json(response)
  } else {
    resp_json <- response
  }

  # process each classification result in the batch
  results <- purrr::map(resp_json, function(item) {
    # extract all label/score pairs
    df <- purrr::map_dfr(item, ~data.frame(
      label = .x$label,
      score = .x$score
    ))

    # pivot to wide format
    tidyr::pivot_wider(df, names_from = label, values_from = score)
  })

  results <- purrr::list_rbind(results)

  return(results)
}

# hf_classify_text_docs ----
#' Classify text using a Hugging Face Inference API endpoint
#'
#' @description
#' Sends text to a Hugging Face classification endpoint and returns the
#' classification scores. By default, returns a tidied data frame with
#' one row and columns for each classification label.
#'
#' @details
#' The text is sent as a batch of one, in the request format for the
#' endpoint's inference engine (see the `engine` argument).
#'
#' On the default Inference Toolkit, `max_length` is sent to the endpoint. TEI
#' ignores it and only cuts texts at the model's own limit, and long texts can
#' give NaN scores, so on TEI EndpointR cuts the text before sending it. With
#' the `tok` package and a `tokenizer`, it cuts at exactly `max_length` tokens;
#' otherwise it cuts at `max_chars` characters.
#'
#' If tidying fails, the function returns the raw response with an
#' informative message.
#'
#' @param text Character string to classify
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#' @param ... Additional arguments passed to `hf_perform_request` and
#'   ultimately to `httr2::req_perform`
#' @param parameters Advanced usage: parameters to pass to the API endpoint.
#'   These override the defaults for the engine.
#' @param tidy Logical; if TRUE (default), returns a tidied data frame
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param timeout Request timeout in seconds
#' @param validate Logical; whether to validate the endpoint before creating
#'   the request
#' @param max_length Maximum number of tokens per text. Longer texts are cut.
#'   `NULL` turns client-side cutting off on TEI.
#' @param max_chars Character limit used on TEI when no tokeniser is available
#' @param tokenizer On TEI: a Hugging Face model id (e.g. `"org/model"`) or a
#'   `tok::tokenizer`, used with the `tok` package to cut texts to
#'   `max_length` tokens. Dedicated endpoints do not report their model id, so
#'   pass it here.
#' @param engine The endpoint's inference engine: `"auto"` (default) detects it
#'   with a call to the endpoint's `/info` route, `"tei"` for Text Embeddings
#'   Inference, `"toolkit"` for the default Hugging Face Inference Toolkit.
#'   Set the default for a session with `options(EndpointR.hf_engine = "tei")`.
#'
#' @return A tidied data frame with classification scores (if `tidy=TRUE`)
#'   or the raw API response
#' @export
#'
#' @examples
#' \dontrun{
#'   result <- hf_classify_text(
#'     text = "This product is excellent!",
#'     endpoint_url = "redacted",
#'     key_name = "API_KEY"
#'   )
#'
#'   # Get raw response without tidying
#'   raw_result <- hf_classify_text(
#'     text = "I love this movie",
#'     endpoint_url = "redacted",
#'     key_name = "API_KEY",
#'     tidy = FALSE
#'   )
#' }
# hf_classify_text docs ----
hf_classify_text <- function(text,
                             endpoint_url,
                             key_name,
                             ...,
                             parameters = list(),
                             tidy = TRUE,
                             max_retries = 5,
                             timeout = 120,
                             validate = FALSE,
                             max_length = 512L,
                             max_chars = 2000L,
                             tokenizer = NULL,
                             engine = getOption("EndpointR.hf_engine", "auto")) {

  stopifnot(
    "Text must be a character vector" = is.character(text)
  )

  if (validate) {
    validate_hf_endpoint(endpoint_url, key_name)
  }

  engine <- .hf_resolve_engine(engine, endpoint_url, key_name)

  send_text <- text
  if (engine$engine == "tei") {
    send_text <- .hf_truncate_for_tei(text, max_length, max_chars, tokenizer, engine, key_name)$texts
  }

  req <- hf_build_request_batch(inputs = send_text,
                                parameters = parameters,
                                endpoint_url = endpoint_url,
                                key_name = key_name,
                                max_retries = max_retries,
                                timeout = timeout,
                                engine = engine$engine,
                                task = "classify",
                                max_length = max_length %||% 512L)

  tryCatch({
    response <- hf_perform_request(req, ...)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to generate classification",
      "i" = "Text: {cli::cli_vec(text, list('vec-trunc' = 30, 'vec-sep' = ''))}",
      "x" = "Error: {conditionMessage(e)}"
    ))
  })

  if (!tidy) { return(response) }

  tryCatch({
    .hf_default_tidy("classify", engine$engine)(response)
  }, error = function(e) {
    cli::cli_alert_info("Failed to tidy output")
    cli::cli_bullets(c(
      "i" = "Text: {cli::cli_vec(text, list('vec-trunc' = 30, 'vec-sep' = ''))}",
      "x" = "Error: {conditionMessage(e)}",
      " " = "Returning un-tidied response, tidy manually."
    ))
    return(response)
  })

}


# hf_classify_batch docs ----
#' Classify multiple texts using Hugging Face Inference Endpoints
#'
#' @description
#' Classifies a batch of texts using a Hugging Face classification endpoint
#' and returns classification scores in a tidy format. Handles batching,
#' concurrent requests, and error recovery automatically.
#'
#' @details
#' Texts are sent in batches of `batch_size`, with `concurrent_requests`
#' requests in flight. On the default Inference Toolkit, texts are sorted by
#' length before batching, because the toolkit pads each batch to its longest
#' text. Results are returned in input order.
#'
#' When a batch fails with a client error (400, 413, 422 or 424) or a network
#' error, it is split in half and sent again, down to single texts, so only the
#' text at fault fails. Requests that get 429 or 5xx are re-sent unchanged, up
#' to `max_retries` times. Empty and missing texts are not sent; they are
#' returned as error rows.
#'
#' On TEI endpoints, EndpointR asks for raw scores and applies the softmax in
#' R, and cuts long texts before sending (see [hf_classify_text()]).
#'
#' The function does not currently handle `list(return_all_scores = FALSE)`.
#'
#' @inheritParams hf_classify_text
#' @param texts Character vector of texts to classify
#' @param ... Reserved for future use
#' @param tidy_func Function to process API responses. `NULL` (default) picks
#'   [tidy_tei_classification_response()] on TEI and
#'   `tidy_batch_classification_response()` on the toolkit. A custom function
#'   receives the response for one batch and must return one row per text.
#' @param batch_size Integer; number of texts per request (default: 32)
#' @param progress Logical; whether to show progress bar (default: TRUE)
#' @param concurrent_requests Integer; number of concurrent requests (default: 16)
#' @param max_retries Integer; maximum re-sends for requests that get 429 or 5xx (default: 5)
#' @param timeout Numeric; request timeout in seconds (default: 120)
#' @param include_texts Logical; whether to include original texts in output
#'   (default: TRUE)
#' @param relocate_col Integer; column position for text column (default: 2)
#'
#' @return Data frame with classification scores for each text, plus columns
#'   for original text (if `include_texts=TRUE`), error status, and error messages
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   texts <- c(
#'     "This product is brilliant!",
#'     "Terrible quality, waste of money",
#'     "Average product, nothing special"
#'   )
#'
#'   results <- hf_classify_batch(
#'     texts = texts,
#'     endpoint_url = "redacted",
#'     key_name = "API_KEY",
#'     batch_size = 32,
#'     concurrent_requests = 16
#'   )
#' }
# hf_classify_batch docs ----
hf_classify_batch <- function(texts,
                              endpoint_url,
                              key_name,
                              ...,
                              tidy_func = NULL,
                              parameters = list(),
                              batch_size = 32,
                              progress = TRUE,
                              concurrent_requests = 16,
                              max_retries = 5,
                              timeout = 120,
                              include_texts = TRUE,
                              relocate_col = 2,
                              max_length = 512L,
                              max_chars = 2000L,
                              tokenizer = NULL,
                              engine = getOption("EndpointR.hf_engine", "auto")) {

  # input validation ----
  if (length(texts) == 0) {
    cli::cli_abort("Input 'texts' is empty.")
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

  engine <- .hf_resolve_engine(engine, endpoint_url, key_name)
  batch_size <- .hf_check_batch_size(batch_size, engine)
  tidy_func <- tidy_func %||% .hf_default_tidy("classify", engine$engine)

  send_texts <- texts
  if (engine$engine == "tei") {
    send_texts <- .hf_truncate_for_tei(texts, max_length, max_chars, tokenizer, engine, key_name)$texts
  }

  processed <- .hf_process_texts(
    texts = send_texts,
    endpoint_url = endpoint_url,
    api_key = api_key,
    engine = engine$engine,
    task = "classify",
    tidy_func = tidy_func,
    batch_size = batch_size,
    concurrent_requests = concurrent_requests,
    max_retries = max_retries,
    timeout = timeout,
    max_length = max_length %||% 512L,
    parameters = parameters,
    progress = progress
  )

  result <- processed$results

  if (include_texts) {
    result$text <- texts[result$.row]
    result <- result |> dplyr::relocate(text, .before = 1)
  }

  result$.row <- NULL

  return(result)
}

# hf_classify_chunks docs ----
#' Efficiently classify vectors of text in chunks
#'
#' @description
#' Classifies large batches of text using a Hugging Face classification endpoint.
#' Processes texts in chunks, sending several texts per request and several
#' requests at once, writes intermediate results to disk as Parquet files, and
#' returns a combined data frame of all classifications.
#'
#' @details
#' The function creates a metadata JSON file in `output_dir` containing processing
#' parameters, the endpoint's inference engine and (on TEI) its limits, how
#' texts were cut, the number of empty texts and the number of split batches.
#' Each chunk is saved as a separate Parquet file before being combined into the
#' final result. Use `output_dir = "auto"` to generate a timestamped directory
#' automatically.
#'
#' See [hf_classify_batch()] for how texts are batched, retried and split, and
#' [hf_classify_text()] for how texts are cut on TEI endpoints. The output's
#' text column holds the original texts, not the cut ones.
#'
#' @inheritParams hf_classify_batch
#' @param ids Vector of unique identifiers corresponding to each text (same length as texts)
#' @param endpoint_url Hugging Face Classification Endpoint
#' @param output_dir Path to directory for the .parquet chunks
#' @param overwrite If `FALSE` (default), errors when `output_dir` already contains chunk (`.parquet`) or `metadata.json` files. Set to `TRUE` to delete them and write fresh outputs; other files are left untouched.
#' @param chunk_size Number of texts to process in each chunk before writing to disk (default: 5000)
#' @param key_name Name of environment variable containing the API key
#' @param id_col_name Name for the ID column in output (default: "id"). When called from hf_classify_df(), this preserves the original column name.
#' @param text_col_name Name for the text column in output (default: "text"). When called from hf_classify_df(), this preserves the original column name.
#'
#' @returns A data frame of classified documents with successes and failures
#' @export
#'
#' @examples
#' \dontrun{
#' texts <- c("I love this", "I hate this", "This is ok")
#' ids <- c("review_1", "review_2", "review_3")
#'
#' results <- hf_classify_chunks(
#'   texts = texts,
#'   ids = ids,
#'   endpoint_url = "https://your-endpoint.huggingface.cloud",
#'   key_name = "HF_API_KEY"
#' )
#' }
# hf_classify_chunks docs ----
hf_classify_chunks <- function(texts,
                               ids,
                               endpoint_url,
                               max_length = 512L,
                               tidy_func = NULL,
                               output_dir = "auto",
                               overwrite = FALSE,
                               chunk_size = 5000L,
                               batch_size = 32L,
                               concurrent_requests = 16L,
                               max_retries = 5L,
                               timeout = 120L,
                               key_name = "HF_API_KEY",
                               id_col_name = "id",
                               text_col_name = "text",
                               max_chars = 2000L,
                               tokenizer = NULL,
                               engine = getOption("EndpointR.hf_engine", "auto"),
                               progress = TRUE
) {

  # input validation ----
  if (length(texts) == 0) {
    cli::cli_abort("Input 'texts' is empty.")
  }

  stopifnot(
    "Texts must be a list or vector" = is.vector(texts),
    "ids must be a vector" = is.vector(ids),
    "texts and ids must be the same length" = length(texts) == length(ids),
    "chunk_size must be a positive integer" = is.numeric(chunk_size) && chunk_size > 0 && chunk_size == as.integer(chunk_size),
    "batch_size must be a positive integer" = is.numeric(batch_size) && batch_size > 0 && batch_size == as.integer(batch_size),
    "concurrent_requests must be a positive integer" = is.numeric(concurrent_requests) && concurrent_requests > 0 && concurrent_requests == as.integer(concurrent_requests),
    "max_retries must be a positive integer" = is.numeric(max_retries) && max_retries >= 0 && max_retries == as.integer(max_retries),
    "timeout must be a positive integer" = is.numeric(timeout) && timeout > 0,
    "endpoint_url must be a non-empty string" = is.character(endpoint_url) && nchar(endpoint_url) > 0,
    "key_name must be a non-empty string" = is.character(key_name) && nchar(key_name) > 0
  )

  # Chunking set up and metadata ----
  output_dir <- .handle_output_directory(output_dir, base_dir_name = "hf_classify_chunk")
  .check_existing_output(output_dir, overwrite = overwrite)

  api_key <- get_api_key(key_name)
  engine <- .hf_resolve_engine(engine, endpoint_url, key_name)
  batch_size <- .hf_check_batch_size(batch_size, engine)
  tidy_func <- tidy_func %||% .hf_default_tidy("classify", engine$engine)

  texts <- unlist(texts)
  if (engine$engine == "tei") {
    cut <- .hf_truncate_for_tei(texts, max_length, max_chars, tokenizer, engine, key_name)
  } else {
    cut <- list(texts = texts, method = if (is.null(max_length)) "none" else "endpoint", n_cut = NA_integer_)
  }
  send_texts <- cut$texts

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  chunk_data <- batch_vector(seq_along(texts), chunk_size)
  n_chunks <- length(chunk_data$batch_indices)

  metadata <- c(
    list(
      output_dir = output_dir,
      endpoint_url = endpoint_url,
      # the body without its inputs
      inference_parameters = hf_batch_body(rep("", batch_size), engine$engine, "classify", max_length %||% 512L)[-1],
      max_length = max_length,
      truncation_method = cut$method,
      n_texts_cut = cut$n_cut,
      chunk_size = chunk_size,
      batch_size = batch_size,
      n_chunks = n_chunks,
      n_texts = length(texts),
      concurrent_requests = concurrent_requests,
      timeout = timeout,
      max_retries = max_retries,
      key_name = key_name,
      timestamp = Sys.time()
    ),
    .hf_engine_metadata(engine)
  )
  .hf_write_metadata(metadata, output_dir)

  cli::cli_alert_info("Processing {length(texts)} text{?s} in {n_chunks} chunk{?s} of up to {chunk_size} rows per chunk, {batch_size} text{?s} per request")
  cli::cli_alert_info("Intermediate results and metadata will be saved as .parquet files and .json in {output_dir}")

  # process chunks ----
  total_successes <- 0
  total_failures <- 0
  total_empty <- 0
  total_splits <- 0

  for (chunk_num in seq_along(chunk_data$batch_indices)) {
    chunk_indices <- chunk_data$batch_indices[[chunk_num]]

    cli::cli_progress_message("Classifying chunk {chunk_num}/{n_chunks} ({length(chunk_indices)} text{?s})")

    processed <- .hf_process_texts(
      texts = send_texts[chunk_indices],
      endpoint_url = endpoint_url,
      api_key = api_key,
      engine = engine$engine,
      task = "classify",
      tidy_func = tidy_func,
      batch_size = batch_size,
      concurrent_requests = concurrent_requests,
      max_retries = max_retries,
      timeout = timeout,
      max_length = max_length %||% 512L,
      progress = progress
    )

    chunk_df <- processed$results
    chunk_df$.chunk <- chunk_num
    chunk_df <- chunk_df |>
      dplyr::mutate(
        !!id_col_name := ids[chunk_indices][.data$.row],
        !!text_col_name := texts[chunk_indices][.data$.row],
        .before = 1
      ) |>
      dplyr::relocate(".chunk", .after = ".status")
    chunk_df$.row <- NULL

    n_chunk_failures <- sum(chunk_df$.error)
    n_chunk_successes <- nrow(chunk_df) - n_chunk_failures
    total_successes <- total_successes + n_chunk_successes
    total_failures <- total_failures + n_chunk_failures
    total_empty <- total_empty + processed$n_empty
    total_splits <- total_splits + processed$n_splits

    .hf_write_chunk(chunk_df, output_dir, chunk_num)

    cli::cli_alert_success("Chunk {chunk_num}: {n_chunk_successes} successful, {n_chunk_failures} failed")
  }

  metadata$n_empty_texts <- total_empty
  metadata$n_split_batches <- total_splits
  .hf_write_metadata(metadata, output_dir)

  # report and return ----
  cli::cli_alert_info("Processing completed, there were {total_successes} successes\n and {total_failures} failures.")

  .hf_read_chunks(output_dir)
}

# hf_classify_df docs ----
#' Classify a data frame of texts using Hugging Face Inference Endpoints
#'
#' @description
#' Classifies texts in a data frame column using a Hugging Face classification
#' endpoint, writing results to disk in chunks.
#'
#' @details
#' This function extracts texts and IDs from the specified columns and
#' classifies them with [hf_classify_chunks()], which writes each chunk to a
#' `.parquet` file in `output_dir` and returns all of the chunks combined.
#'
#' See [hf_classify_batch()] for how texts are batched, retried and split, and
#' [hf_classify_text()] for how texts are cut on TEI endpoints.
#'
#' The function does not currently handle `list(return_all_scores = FALSE)`.
#'
#' @inheritParams hf_classify_chunks
#' @param df Data frame containing texts to classify
#' @param text_var Column name containing texts to classify (unquoted)
#' @param id_var Column name to use as identifier for joining (unquoted)
#' @param key_name Name of environment variable containing the API key
#' @param chunk_size Number of texts to process in each chunk before writing to disk (default: 5000)
#' @param concurrent_requests Integer; number of concurrent requests (default: 16)
#'
#' @return A data frame with the ids, texts and classification scores, plus
#'   `.error`, `.error_msg`, `.status` and `.chunk` columns
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(
#'     id = 1:3,
#'     review = c("Excellent service", "Poor quality", "Average experience")
#'   )
#'
#'   classified_df <- hf_classify_df(
#'     df = df,
#'     text_var = review,
#'     id_var = id,
#'     endpoint_url = "redacted",
#'     key_name = "API_KEY",
#'     batch_size = 32,
#'     concurrent_requests = 16
#'   )
#' }
# hf_classify_df docs ----
hf_classify_df <- function(df,
                           text_var,
                           id_var,
                           endpoint_url,
                           key_name,
                           max_length = 512L,
                           output_dir = "auto",
                           overwrite = FALSE,
                           tidy_func = NULL,
                           chunk_size = 5000,
                           batch_size = 32L,
                           concurrent_requests = 16,
                           max_retries = 5,
                           timeout = 120,
                           max_chars = 2000L,
                           tokenizer = NULL,
                           engine = getOption("EndpointR.hf_engine", "auto"),
                           progress = TRUE) {

  # mirrors the hf_embed_df function
  text_sym <- rlang::ensym(text_var)
  id_sym <- rlang::ensym(id_var)

  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
    "concurrent_requests must be a number greater than 0" = is.numeric(concurrent_requests) && concurrent_requests > 0,
    "chunk_size must be a number greater than 0" = is.numeric(chunk_size) && chunk_size > 0
  )

  output_dir <- .handle_output_directory(output_dir, base_dir_name = "hf_classification_chunks")

  # pull texts & ids into vectors for batch function
  text_vec <- dplyr::pull(df, !!text_sym)
  indices_vec <- dplyr::pull(df, !!id_sym)

  # preserve original column names
  id_col_name <- rlang::as_name(id_sym)
  text_col_name <- rlang::as_name(text_sym)

  chunk_size <- if (is.null(chunk_size) || chunk_size <= 1) 1 else chunk_size

  results <- hf_classify_chunks(
    texts = text_vec,
    ids = indices_vec,
    endpoint_url = endpoint_url,
    max_length = max_length,
    tidy_func = tidy_func,
    chunk_size = chunk_size,
    batch_size = batch_size,
    concurrent_requests = concurrent_requests,
    max_retries = max_retries,
    timeout = timeout,
    key_name = key_name,
    output_dir = output_dir,
    overwrite = overwrite,
    id_col_name = id_col_name,
    text_col_name = text_col_name,
    max_chars = max_chars,
    tokenizer = tokenizer,
    engine = engine,
    progress = progress
  )

  return(results)
}
