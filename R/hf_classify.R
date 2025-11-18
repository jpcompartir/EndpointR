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
#' @param output_dir Path to directory for the .parquet chunks
# hf_classify_chunks docs ----
hf_classify_chunks <- function(texts,
                               ids,
                               endpoint_url,
                               ...,
                               tidy_func = tidy_classification_response,
                               output_dir = "auto",
                               chunk_size = 5000L,
                               concurrent_requests = 5L,
                               max_retries = 5L,
                               timeout = 30L,
                               include_texts = TRUE,
                               relocate_col = 2,
                               key_name = "HF_API_KEY"
) {

}
#' Classify a data frame of texts using Hugging Face Inference Endpoints
#'
#' @description
#' Classifies texts in a data frame column using a Hugging Face classification
#' endpoint and joins the results back to the original data frame.
#'
#' @details
#' This function extracts texts from a specified column, classifies them using
#' `hf_classify_batch()`, and joins the classification results back to the
#' original data frame using a specified ID column.
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


