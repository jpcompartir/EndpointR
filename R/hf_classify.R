# space for classifying text data with HF Inference Endpoints
# functions from core and hf_inference will be helpful to re-use
# functions from hf_embed serve as :sparkles: inspo :sparkles:

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
tidy_batch_classification <- function(response) {
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

# hf_classify_docs ----
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
#' @param parameters List of parameters to pass to the API endpoint,
#'   defaults to `list(return_all_scores = TRUE)`
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
#'     endpoint_url = "https://api-inference.huggingface.co/models/distilbert-base-uncased-finetuned-sst-2-english"
#'   )
#'
#'   # Classification with custom parameters for a spam detection model
#'   spam_result <- hf_classify_text(
#'     text = "URGENT: You've won a free holiday! Call now to claim.",
#'     endpoint_url = "https://api-inference.huggingface.co/models/mrm8488/bert-tiny-finetuned-sms-spam-detection",
#'     parameters = list(return_all_scores = TRUE, wait_for_model = TRUE)
#'   )
#'
#'   # Get raw response without tidying
#'   raw_result <- hf_classify_text(
#'     text = "I love this movie",
#'     endpoint_url = "https://api-inference.huggingface.co/models/distilbert-base-uncased-finetuned-sst-2-english",
#'     tidy = FALSE
#'   )
#' }
# hf_classify_docs ----
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

  req <-  req <- hf_build_request(input = text,
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


  if (!tidy) { return(response)}

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

