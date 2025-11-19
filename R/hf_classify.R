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
#' This function handles the entire process of creating a request to a
#' Hugging Face Inference API endpoint for text classification, sending
#' the request, and processing the response.
#'
#' The function will automatically retry failed requests according to the
#' `max_retries` parameter. If `tidy=TRUE` (the default), it transforms
#' the nested JSON response into a tidy data frame with one row and columns
#' for each classification label.
#'
#' If tidying fails, the function returns the raw response with an
#' informative message.
#'
#' @param text Character string to classify
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#' @param ... Additional arguments passed to `hf_perform_request` and
#'   ultimately to `httr2::req_perform`
#' @param parameters Advanced usage: parameters to pass to the API endpoint,
#'   defaults to `list(return_all_scores = TRUE)`.
#' @param tidy Logical; if TRUE (default), returns a tidied data frame
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param timeout Request timeout in seconds
#' @param validate Logical; whether to validate the endpoint before creating
#'   the request
#'
#' @return A tidied data frame with classification scores (if `tidy=TRUE`)
#'   or the raw API response
#' @export
#'
#' @examples
#' \dontrun{
#'   # Basic classification with default parameters
#'   result <- hf_classify_text(
#'     text = "This product is excellent!",
#'     endpoint_url = "redacted",
#'     key_name = "API_KEY"
#'   )
#'
#'   # Classification with custom parameters for a spam detection model
#'   spam_result <- hf_classify_text(
#'     text = "URGENT: You've won a free holiday! Call now to claim.",
#'     endpoint_url = "redacted",
#'     parameters = list(return_all_scores = TRUE)
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
                             parameters = list(return_all_scores = TRUE),
                             tidy = TRUE,
                             max_retries = 5,
                             timeout = 20,
                             validate = FALSE
                             ) {

  stopifnot(
    "Text must be a character vector" = is.character(text)
  )
  api_key <- get_api_key(key_name)

  req <- hf_build_request(input = text,
                          parameters = parameters,
                          endpoint_url = endpoint_url,
                          key_name = key_name,
                          max_retries = max_retries,
                          timeout = timeout,  # longer for classification than embedding default
                          validate = validate)

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
    tidy_classification_response(response)
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
#' This function processes multiple texts efficiently by splitting them into
#' batches and optionally sending concurrent requests. It includes robust
#' error handling and progress reporting for large batches.
#'
#' The function automatically handles request failures with retries and
#' includes error information in the output when requests fail. Original
#' text order is preserved in the results.
#'
#' The function does not currently handle `list(return_all_scores = FALSE)`.
#'
#' @param texts Character vector of texts to classify
#' @param endpoint_url URL of the Hugging Face Inference API endpoint
#' @param key_name Name of environment variable containing the API key
#' @param ... Additional arguments passed to request functions
#' @param tidy_func Function to process API responses, defaults to
#'   `tidy_batch_classification_response`
#' @param parameters List of parameters for the API endpoint, defaults to
#'   `list(return_all_scores = TRUE)`
#' @param batch_size Integer; number of texts per batch (default: 8)
#' @param progress Logical; whether to show progress bar (default: TRUE)
#' @param concurrent_requests Integer; number of concurrent requests (default: 5)
#' @param max_retries Integer; maximum retry attempts (default: 5)
#' @param timeout Numeric; request timeout in seconds (default: 20)
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
#'     batch_size = 3
#'   )
#' }
# hf_classify_batch docs ----
hf_classify_batch <- function(texts,
                              endpoint_url,
                              key_name,
                              ...,
                              tidy_func = tidy_batch_classification_response,
                              parameters = list(return_all_scores = TRUE),
                              batch_size = 8,
                              progress = TRUE,
                              concurrent_requests = 5,
                              max_retries = 5,
                              timeout = 30,
                              include_texts = TRUE,
                              relocate_col = 2
                              ){

  # mirrors hf_embed_batch

  # input validation ----
  if (length(texts) == 0) {
    cli::cli_abort("Input 'texts' is empty or . Returning an empty tibble.")
  }

  if (length(texts) == 1) {
    cli::cli_abort("Function expects a batch of inputs, use `hf_classify_text` for single texts.")
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

  batch_reqs <- purrr::map(batch_data$batch_inputs,
                           ~hf_build_request_batch(.x,
                                                   endpoint_url,
                                                   key_name,
                                                   parameters = parameters,
                                                   max_retries = max_retries,
                                                   timeout = timeout,
                                                   validate = FALSE))

  # performing requests ----
  if (length(batch_reqs) == 1){ # single batch case
    # browser()
    response <- safely_perform_request(batch_reqs[[1]])

    if (!is.null(response$result) && response$result$status_code == 200) { # success = try to tidy

      tryCatch({ # if we can't tidy, flag errors
        result <- tidy_func(response$result)
        result$original_index <- batch_data$batch_indices[[1]]
        result$.error <- FALSE
        result$.error_msg <- NA_character_
      }, error = function(e) {
        cli::cli_warn("Error in single batch request: {conditionMessage(e)}")
        return(.create_error_tibble(batch_data$batch_indices, conditionMessage(e)))
      })

    } else {
      result <- .create_error_tibble(batch_data$batch_indices, response$error)
    }

  } else { # multiple batch case

    response_list <- perform_requests_with_strategy(
      requests = batch_reqs,
      concurrent_requests = concurrent_requests,
      progress = progress # todo: possibly parameter?
    )

    # map through the responses and tidy them.
    processed_responses <- purrr::map2(
      response_list, batch_data$batch_indices,
      ~process_response(.x, .y, tidy_func)
    )
    result <- purrr::list_rbind(processed_responses)
  }

  result <- dplyr::arrange(result, original_index)

  if (include_texts) {
    result$text <- texts[result$original_index]
    result <- result |>  dplyr::relocate(text, .before = 1)
  }

  result$original_index <- NULL # drop index now we're returning

  return(result)
}

# hf_classify_chunks docs ----
#' Efficiently classify vectors of text in chunks
#'
#' TODO - description
#'
#' TODO - details
#'
#'
#' @param texts Character vector of texts to classify
#' @param ids Vector of unique identifiers corresponding to each text (same length as texts)
#' @param endpoint_url Hugging Face Embedding Endpoint
#' @param max_length The maximum number of tokens in the text variable. Beyond this cut-off everything is truncated.
#' @param tidy_func Function to process API responses, defaults to
#'   `tidy_classification_response`
#' @param output_dir Path to directory for the .parquet chunks
#' @param chunk_size Number of texts to process in each chunk before writing to disk (default: 5000)
#' @param concurrent_requests Integer; number of concurrent requests (default: 5)
#' @param max_retries Integer; maximum retry attempts (default: 5)
#' @param timeout Numeric; request timeout in seconds (default: 20)
#' @param key_name Name of environment variable containing the API key
#'
#' @returns A data frame of classified documents with successes and failures
#' @export
#'
#' @examples
#' \dontrun{
#' 1+1 = 2
#' }
# hf_classify_chunks docs ----
hf_classify_chunks <- function(texts,
                               ids,
                               endpoint_url,
                               max_length = 512L,
                               tidy_func = tidy_classification_response,
                               output_dir = "auto",
                               chunk_size = 5000L,
                               concurrent_requests = 5L,
                               max_retries = 5L,
                               timeout = 30L,
                               key_name = "HF_API_KEY"
) {

  # input validation ----
  if (length(texts) == 0) {
    cli::cli_abort("Input 'texts' is empty or . Returning an empty tibble.")
  }

  if (length(texts) == 1) {
    cli::cli_abort("Function expects a batch of inputs, use `hf_classify_text` for single texts.")
  }


  stopifnot(
    "Texts must be a list or vector" = is.vector(texts),
    "ids must be a vector" = is.vector(ids),
    "texts and ids must be the same length" = length(texts) == length(ids),
    "chunk_size must be a positive integer" = is.numeric(chunk_size) && chunk_size > 0 && chunk_size == as.integer(chunk_size),
    "concurrent_requests must be a positive integer" = is.numeric(concurrent_requests) && concurrent_requests > 0 && concurrent_requests == as.integer(concurrent_requests),
    "max_retries must be a positive integer" = is.numeric(max_retries) && max_retries >= 0 && max_retries == as.integer(max_retries),
    "timeout must be a positive integer" = is.numeric(timeout) && timeout > 0,
    "endpoint_url must be a non-empty string" = is.character(endpoint_url) && nchar(endpoint_url) > 0,
    "key_name must be a non-empty string" = is.character(key_name) && nchar(key_name) > 0
  )

  # core logic ----
  output_dir <- .handle_output_directory(output_dir, base_dir_name = "hf_classify_chunk")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  chunk_data <- batch_vector(seq_along(texts), chunk_size)
  n_chunks <- length(chunk_data$batch_indices)

  inference_parameters = list(return_all_scores = TRUE,
                    truncation = TRUE,
                    max_length = max_length)

  metadata <- list(
    endpoint_url = endpoint_url,
    chunk_size = chunk_size,
    n_texts = length(texts),
    concurrent_requests = concurrent_requests,
    timeout = timeout,
    output_dir = output_dir,
    key_name = key_name,
    n_chunks = n_chunks,
    timestamp = Sys.time(),
    inference_parameters = inference_parameters
  )

  jsonlite::write_json(metadata,
                       file.path(output_dir, "metadata.json"),
                       auto_unbox = TRUE,
                       pretty = TRUE)

  cli::cli_alert_info("Processing {length(texts)} text{?s} in {n_chunks} chunk{?s} of up to {chunk_size} rows per chunk")
  cli::cli_alert_info("Intermediate results and metadata will be saved as .parquet files and .json in {output_dir}")

  # track global successes for failures for end-of-pipeline reporting
  total_successes <- 0
  total_failures <- 0

  for (chunk_num in seq_along(chunk_data$batch_indices)) {
    chunk_indices <- chunk_data$batch_indices[[chunk_num]]
    chunk_texts <- texts[chunk_indices]
    chunk_ids <- ids[chunk_indices]

    cli::cli_progress_message("Classifying chunk {chunk_num}/{n_chunks} ({length(chunk_indices)} text{?s})")

    requests <- purrr::map2(
      .x = chunk_texts,
      .y = chunk_ids,
      .f = \(x, y) hf_build_request(
        input = x,
        endpoint_url = endpoint_url,
        endpointr_id = y,
        key_name = key_name,
        parameters = inference_parameters,
        max_retries = max_retries,
        timeout = timeout,
        validate = FALSE
      )
    )

    is_valid_request <- purrr::map_lgl(requests, \(x) inherits(x, "httr2_request"))

    valid_requests <- requests[is_valid_request]

    if (length(valid_requests) == 0) {
      cli::cli_alert_warning("No valid request{?s} in chunk {chunk_num}, skipping")
      next
    }

    responses <- perform_requests_with_strategy(
      valid_requests,
      concurrent_requests = concurrent_requests,
      progress = TRUE
    )

    chunk_successes <- httr2::resps_successes(responses)
    chunk_failures <- httr2::resps_failures(responses)

    n_chunk_successes <- length(chunk_successes)
    n_chunk_failures <- length(chunk_failures)

    total_successes <- total_successes + n_chunk_successes
    total_failures <- total_failures + n_chunk_failures

    chunk_results <- list()

    if (n_chunk_successes > 0) {

      successes_ids <- purrr::map(chunk_successes, \(x) purrr::pluck(x, "request", "headers", "endpointr_id")) |>
        unlist()
      successes_texts <- purrr::map(chunk_successes, \(x) purrr::pluck(x, "request", "body", "data", "inputs")) |>  unlist()
      successes_content <- purrr::map(chunk_successes, tidy_func) |>
        purrr::list_rbind()

      chunk_results$successes <- tibble::tibble(
        id = successes_ids,
        text = successes_texts,
        .error = FALSE,
        .error_msg = NA_character_,
        .chunk = chunk_num
      ) |>
        dplyr::bind_cols(successes_content)

    }

    if (n_chunk_failures > 0) {

      failures_ids <- purrr::map(chunk_failures, \(x) purrr::pluck(x, "request", "headers", "endpointr_id")) |>  unlist()
      failures_texts <- purrr::map_chr(chunk_failures, \(x) purrr::pluck(x, "request", "body", "data", "inputs")) |> unlist()
      failures_msgs <- purrr::map_chr(chunk_failures, \(x) purrr::pluck(x, "message", .default = "Unknown error"))


      chunk_results$failures <- tibble::tibble(
        id = failures_ids,
        text = failures_texts,
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

    cli::cli_alert_success("Chunk {chunk_num}: {n_chunk_successes} successful, {n_chunk_failures} failed")
  }

  parquet_files <- list.files(output_dir, pattern = "\\.parquet$", full.names = TRUE)

  cli::cli_alert_info("Processing completed, there were {total_successes} successes\n and {total_failures} failures.")
  final_results <- arrow::open_dataset(parquet_files, format = "parquet") |>
    dplyr::collect()

  return(final_results)
}

# hf_classify_df docs ----
#' Classify a data frame of texts using Hugging Face Inference Endpoints
#'
#' @description
#' Classifies texts in a data frame column using a Hugging Face classification
#' endpoint and joins the results back to the original data frame.
#'
#' @details
#' This function extracts texts and IDs from the specified columns, classifies them in chunks.
#' It writes
#' `hf_classify_chunks()`, and then returns all of the chu
#'
#' The function preserves the original data frame structure and adds new
#' columns for classification scores. If the number of rows doesn't match
#' after processing (due to errors), it returns the classification results
#' separately with a warning.
#'
#' The function does not currently handle `list(return_all_scores = FALSE)`.
#'
#' @param df Data frame containing texts to classify
#' @param text_var Column name containing texts to classify (unquoted)
#' @param id_var Column name to use as identifier for joining (unquoted)
#' @param endpoint_url URL of the Hugging Face Inference API endpoint
#' @param key_name Name of environment variable containing the API key
#' @param ... Additional arguments passed to request functions
#' @param tidy_func Function to process API responses, defaults to
#'   `tidy_batch_classification_response`
#' @param parameters List of parameters for the API endpoint, defaults to
#'   `list(return_all_scores = TRUE)`
#' @param batch_size Integer; number of texts per batch (default: 4)
#' @param concurrent_requests Integer; number of concurrent requests (default: 1)
#' @param max_retries Integer; maximum retry attempts (default: 5)
#' @param timeout Numeric; request timeout in seconds (default: 30)
#' @param progress Logical; whether to show progress bar (default: TRUE)
#'
#' @return Original data frame with additional columns for classification scores,
#'   or classification results table if row counts don't match
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
#'     key_name = "API_KEY"
#'   )
#' }
# hf_classify_df docs ----
hf_classify_df <- function(df,
                           text_var,
                           id_var,
                           endpoint_url,
                           key_name,
                           ...,
                           tidy_func = tidy_batch_classification_response,
                           parameters = list(return_all_scores = TRUE),
                           batch_size = 4,
                           concurrent_requests = 1,
                           max_retries = 5,
                           timeout = 30,
                           progress = TRUE) {


  # mirrors the hf_embed_df function
  text_sym <- rlang::ensym(text_var)
  id_sym <- rlang::ensym(id_var)

  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
    "concurrent_requests must be a number greater than 0" = is.numeric(concurrent_requests) && concurrent_requests > 0,
    "batch_size must be a number greater than 0" = is.numeric(batch_size) && batch_size > 0
  )

  original_num_rows <- nrow(df) # for final sanity check

  # pull texts & ids into vectors for batch function
  text_vec <- dplyr::pull(df, !!text_sym)
  indices_vec <- dplyr::pull(df, !!id_sym)

  batch_size <- if(is.null(batch_size) || batch_size <=1) 1 else batch_size

  classification_tbl <- hf_classify_batch(texts = text_vec,
                                          endpoint_url = endpoint_url,
                                          key_name = key_name,
                                          tidy_func = tidy_func,
                                          parameters = parameters,
                                          batch_size = batch_size,
                                          max_retries = max_retries,
                                          timeout = timeout,
                                          progress = TRUE,
                                          concurrent_requests = concurrent_requests)


  final_num_rows <- nrow(classification_tbl)

  if(final_num_rows == original_num_rows) {
    classification_tbl <- classification_tbl |> dplyr::mutate(!!id_sym := indices_vec)

    df <- dplyr::left_join(df, classification_tbl)

    return(df)
  } else {
    cli::cli_warn("Rows in original data frame and returned data frame do not match:")
    cli::cli_bullets(text = c(
      "Rows in original data frame: {original_num_rows}",
      "Rows in returned data frame: {final_num_rows}"
    ))
    cli::cli_alert_info("Returning table with all available response data")
    return(classification_tbl)
  }

}


