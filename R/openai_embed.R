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


# oai_build_embedding_request ----
# Cannot input empty string
# Inputs (combined) cannot exceed 8092 tokens (max embed. length)
# Can input dimensions
# Don't need the batch embedding request for OpenAI really, at all.
oai_build_embedding_request <- function(input, model = "text-embedding-3-small", dimensions = NULL, max_retries = 5, timeout = 20, endpoint_url = "https://api.openai.com/v1/embeddings", key_name = "OPENAI_API_KEY") {

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

  attr(batch_request, "total_chars") <- total_chars # so caller func(s) can access this and raise a warning if need be(?) may deprecate

  return(batch_request)

}


# oai_embed_text ----
# Checks that text is a length 1 vector, i.e. not batch
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


# oai_embed_batch ----
# Checks texts is a length > 1 vector, i.e. not single text
oai_embed_batch <- function(texts,
                            model = "text-embedding-3-small",
                            dimensions = NULL,
                            batch_size = 10,
                            concurrent_requests = 1,
                            max_retries = 5,
                            timeout = 20,
                            endpoint_url = "https://api.openai.com/v1/embeddings",
                            key_name = "OPENAI_API_KEY",
                            tidy_func = tidy_oai_embedding,
                            include_texts = TRUE,
                            relocate_col = 2) {


  if (length(texts) == 0) {
    cli::cli_warn("Input 'texts' is empty. Returning an empty tibble.")
    return(tibble::tibble())
  }

  stopifnot(
    "Texts must be a list or vector" = is.vector(texts),
    "Texts must have length > 1" = length(texts) > 1,
    "batch_size must be a positive integer" = is.numeric(batch_size) && batch_size > 0,
    "concurrent_requests must be a positive integer" = is.numeric(concurrent_requests) && concurrent_requests > 0
  )

  text_classes <- purrr::map(texts, class)
  text_not_empty <- purrr::map_lgl(texts, ~ .x != "")

  stopifnot("Each individual text must be a character vector" = all(text_classes == "character"),
            "Text must not be an empty string" = all(text_not_empty))

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
      concurrent_requests = concurrent_requests
    )
  )

  response_list <- perform_requests_with_strategy(
    requests = batch_requests,
    concurrent_requests = concurrent_requests,
    progress = TRUE
  )

  processed_responses <- purrr::map2(
    response_list,
    batch_data$batch_indices,
    ~ process_response(.x, .y, tidy_func)
  )

  result <- purrr::list_rbind(processed_responses)
  result <- dplyr::arrange(result, original_index)

  if (include_texts) {
    result$text <- texts[result$original_index]
    result <- dplyr::relocate(result, text, .before = 1)
  }

  result$original_index <- NULL

  if (all(c(".error", ".error_message") %in% names(result))) {
    result <- dplyr::relocate(result, c(.error, .error_message), .before = dplyr::all_of(relocate_col))
  }

  return(result)
}


oai_embed_df <- function(df, text_var, id_var, model = "text-embedding-3-small", dimensions = NULL, batch_size = 1, concurrent_requests = 1, max_retries = 5, timeout = 20, endpoint_url = "https://api.openai.com/v1/embeddings", key_name = "OPENAI_API_KEY" ) {

}



# helper funcs (may move to utils or something) ----
parse_oai_date <- function(date_string) {
  parsed_date <- as.POSIXct(date_string, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  date <- as.Date(parsed_date)
  return(date)
}


