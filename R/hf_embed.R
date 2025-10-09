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
#' @param text Character string to get embeddings for
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#' @param ... ellipsis sent to `hf_perform_request`, which forwards to `httr2::req_perform`
#' @param parameters Advanced usage: parameters to pass to the API endpoint
#' @param tidy Whether to attempt to tidy the response or not
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
                         parameters = list(),
                         tidy = TRUE,
                         max_retries = 3,
                         timeout = 10,
                         validate = FALSE) {

  stopifnot(
    "Text must be a character vector" = is.character(text)
  )

  req <- hf_build_request(input = text,
                          parameters = parameters,
                          endpoint_url = endpoint_url,
                          key_name = key_name,
                          max_retries = max_retries,
                          timeout = timeout,
                          validate = validate)

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

  if(tidy){
    response <- tidy_embedding_response(response)
  }

  return(response)
}


# hf_embed_batch docs ----
#' Generate batches of embeddings for a list of texts
#'
#' @description
#' High-level function to generate embeddings for multiple text strings.
#' This function handles batching and parallel processing of embedding requests, and attempts to handle errors gracefully.
#'
#' @param texts Vector or list of character strings to get embeddings for
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#' @param ... ellipsis sent to `hf_perform_request` TODO (reserved ATM)
#' @param tidy_func Function to process/tidy the raw API response (default: tidy_embedding_response)
#' @param parameters Advanced usage: parameters to pass to the API endpoint.
#' @param batch_size Number of texts to process in one batch
#' @param include_texts Whether to return the original texts in the return tibble
#' @param concurrent_requests Number of requests to send simultaneously
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param timeout Request timeout in seconds
#' @param validate Whether to validate the endpoint before creating the request
#' @param relocate_col Which position in the data frame to relocate the results to.
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
#'     concurrent_requests = 5
#'   )
#' }
# hf_embed_batch docs ----
hf_embed_batch <- function(texts,
                           endpoint_url,
                           key_name,
                           ...,
                           tidy_func = tidy_embedding_response,
                           parameters = list(),
                           batch_size = 8,
                           include_texts = TRUE,
                           concurrent_requests = 5,
                           max_retries = 5,
                           timeout = 10,
                           validate = FALSE,
                           relocate_col = 2){


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

  # preparing batches ----
  batch_data <- batch_vector(texts, batch_size) # returns $batch_indices, $batch_inputs

  batch_reqs <- purrr::map(
    batch_data$batch_inputs,
    ~hf_build_request_batch(.x,
                            endpoint_url,
                            key_name,
                            parameters = parameters,
                            max_retries = max_retries,
                            timeout = timeout,
                            validate = FALSE))

  response_list <- perform_requests_with_strategy(
    requests = batch_reqs,
    concurrent_requests = concurrent_requests,
    progress = TRUE # TODO do we want a parameter here?
  )

  processed_responses <- purrr::map2(
    response_list,
    batch_data$batch_indices,
    ~process_response(.x, .y, tidy_func)
  )

  # formatting results ----
  result <- purrr::list_rbind(processed_responses)
  result <- dplyr::arrange(result, original_index)

  if (include_texts) {
    result$text <- texts[result$original_index]
    result <- result |>  dplyr::relocate(text, .before = 1)
  }

  result$original_index <- NULL # drop index now we're returning

  result <- dplyr::relocate(result, c(`.error`, `.error_message`), .before = dplyr::all_of(relocate_col))
  return(result)
}


#' Embed text chunks through Hugging Face Inference Embedding Endpoints
#'
#' This function is capable of processing large volumes of text through Hugging Face's Inference Embedding Endpoints. Results are written in batches to a file, to avoid out of memory issues.
#'
#'
#' @param texts Character vector of texts to process
#' @param ids Vector of unique identifiers corresponding to each text (same length as texts)
#' @param endpoint_url Hugging Face Embedding Endpoint
#' @param output_file Path to .CSV file for results. "auto" generates the filename, location and is persistent across sessions. If NULL, generates timestamped filename.
#' @param chunk_size Number of texts to process in each batch (default: 5000)
#' @param concurrent_requests number of concurrent requests (default: 5)
#' @param max_retries aximum retry attempts per failed request (default: 5)
#' @param timeout Request timeout in seconds (default: 30)
#' @param key_name ame of environment variable containing the API key (default:
#' "HF_API_KEY")
#'
#' @returns
#' @export
#'
#' @examples
hf_embed_chunks <- function(texts,
                            ids,
                            endpoint_url,
                            output_file = "auto",
                            chunk_size = 5000L,
                            concurrent_requests = 5L,
                            max_retries = 5L,
                            timeout = 10L,
                            key_name = "HF_API_KEY") {

  # to batch_size or not to batch_size? As there were some problems with 1 item in the batch failing leading to the whole batch failing, we'll start without.
  # input validation ----
  stopifnot(
    "texts must be a vector" = is.vector(texts),
    "ids must be a vector" = is.vector(ids),
    "texts and ids must be the same length" = length(texts) == length(ids),
    "chunk_size must be a positive integer greater than 1" = is.numeric(chunk_size) && chunk_size > 0
  )

  output_file = .handle_output_filename(output_file)

  batch_data <- batch_vector(seq_along(texts), chunk_size)
  n_batches <- length(batch_data$batch_indices)

  cli::cli_alert_info("Processing {length(texts)} text{?s} in {n_batches} chunk{?s} of up to {chunk_size} rows each")
  cli::cli_alert_info("Intermediate results will be saved to a .csv at {output_file}.")

  total_success <- 0
  total_failures <- 0

  ## Batch Processing ----
  for (batch_num in seq_along(batch_data$batch_indices))
  {
    batch_indices <- batch_data$batch_indices[[batch_num]]
    batch_texts <- texts[batch_indices]
    batch_ids <- ids[batch_indices]

    cli::cli_progress_message("Processing batch {batch_num}/{n_batches} ({length(batch_indices)} text{?s})")

    requests <- purrr::map2(
      .x = batch_texts,
      .y = batch_ids,
      .f = \(x, y) hf_build_request(
        input = x,
        endpoint_url = endpoint_url,
        endpointr_id = y,
        key_name = key_name,
        parameters = list(),
        max_retries = max_retries,
        timeout = timeout,
        validate = FALSE
      )
    )

    is_valid_request <- purrr::map_lgl(requests, \(x) inherits(x, "httr2_request"))
    valid_requests <- requests[is_valid_request]

    if (length(valid_requests) == 0) {
      cli::cli_alert_warning("No valid request{?s} in batch {batch_num}, skipping")
    }

    responses <- perform_requests_with_strategy(
      valid_requests,
      concurrent_requests = concurrent_requests,
      progress = TRUE
    )

    successes <- httr2::resps_successes(responses)
    failures <- httr2:::resps_failures(responses)

    n_successes <- length(successes)
    n_failures <- length(failures)
    total_success <- total_success + n_successes
    total_failures <- total_failures + n_failures

    # within batch results ----
    batch_results <- list()

    if (length(successes) >0){
      successes_ids <- purrr::map(successes, \(x) purrr::pluck(x, "request", "headers", "endpointr_id")) |>  unlist()
      successes_content <- purrr::map(successes, tidy_embedding_response) |>
        purrr::list_rbind()

      batch_results$successes <- tibble::tibble(
        id = successes_ids,
        .error = FALSE,
        .error_msg = NA_character_,
        .batch = batch_num
      ) |>
        dplyr::bind_cols(successes_content)
    }

    if (length(failures) > 0) {
      failures_ids <- purrr::map(failures, \(x) pluck(x, "request", "headers", "endpointr_id")) |>  unlist()
      failures_msgs <- purrr::map_chr(failures, \(x) purrr::pluck(x, "message", .default = "Unknown error"))

      batch_results$failures <- tibble::tibble(
        id = failures_ids,
        .error = TRUE,
        .error_msg = failures_msgs,
        .batch = batch_num
      )
    }

    batch_df <- dplyr::bind_rows(batch_results)

    if (nrow(batch_df) > 0) {
      if (batch_num == 1) {
        # if we're in the first batch write to csv with headers (col names)
        readr::write_csv(batch_df, output_file, append = FALSE)
      } else {
        # all other batches, append and don't use col names
        readr::write_csv(batch_df, output_file, append = TRUE, col_names = FALSE)}
    }

    cli::cli_alert_success("Batch {batch_num}: {n_successes} successful, {n_failures} failed")
  }

  final_results <- readr::read_csv(output_file, show_col_types = FALSE)

  return(final_results)
}


# hf_embed_df docs ----
#' Generate embeddings for texts in a data frame
#'
#' @description
#' High-level function to generate embeddings for texts in a data frame.
#' This function handles the entire process from request creation to
#' response processing, with options for batching & parallel execution.
#' Setting the number of retries
#'
#' @param df A data frame containing texts to embed
#' @param text_var Name of the column containing text to embed
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param id_var Name of the column to use as ID
#' @param key_name Name of the environment variable containing the API key
#' @param batch_size Number of texts to process in one batch (NULL for no batching)
#' @param concurrent_requests Number of requests to send at once. Some APIs do not allow for multiple requests.
#' @param max_retries Maximum number of retry attempts for failed requests.
#' @param timeout Request timeout in seconds
#' @param progress Whether to display a progress bar
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
                        id_var,
                        endpoint_url,
                        key_name,
                        batch_size = 8,
                        concurrent_requests = 1,
                        max_retries = 5,
                        timeout = 15,
                        progress = TRUE) {

  text_sym <- rlang::ensym(text_var)
  id_sym <- rlang::ensym(id_var)

  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    # "df must be a data frame with > 0 rows", nrow(df) > 0,
    "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
    "concurrent_requests must be an integer" = is.numeric(concurrent_requests) && concurrent_requests > 0
  )

  if (!rlang::as_string(text_sym) %in% names(df)) {
    cli::cli_abort("Column {.code {rlang::as_string(text_sym)}} not found in data frame")
  }

  if (!rlang::as_string(id_sym) %in% names(df)) {
    cli::cli_abort("Column {.code {rlang::as_string(id_sym)}} not found in data frame")
  }

  original_num_rows <- nrow(df)

  # refactoring  to always use hf_embed_batch - if batch_size if one then it gets handled anyway, avoids branching and additional complexity.
  texts <- dplyr::pull(df, !!text_sym)
  indices <- dplyr::pull(df, !!id_sym)

  batch_size <- if(is.null(batch_size) || batch_size <= 1) 1 else batch_size

  embeddings_tbl <- hf_embed_batch(
    texts = texts,
    endpoint_url = endpoint_url,
    key_name = key_name,
    batch_size = batch_size,
    include_texts = FALSE,
    concurrent_requests = concurrent_requests,
    max_retries = max_retries,
    timeout = timeout,
    validate = FALSE,
    relocate_col = 1
  )

  df_with_row_id <- df |> dplyr::mutate(.row_id = dplyr::row_number()) # do we definitely want to copy this df? It could be large

  embeddings_tbl <- embeddings_tbl |>
    dplyr::mutate(.row_id = dplyr::row_number())

  result_df <- df_with_row_id |>
    dplyr::left_join(embeddings_tbl, by = ".row_id") |>
    dplyr::select(-.row_id)


  # final sanity check and alert user if there's a mismatch
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

