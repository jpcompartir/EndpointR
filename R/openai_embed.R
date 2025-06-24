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


# oai_build_embedding_request ----
# Cannot input empty string
# Inputs (combined) cannot exceed 8092 tokens (max embed. length)
# Can input dimensions
# Don't need the batch embedding request for OpenAI really, at all.
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
# If a batch fails, all docs in that *batch* fails. I.e. not all docs in all batches, just docs in that batch.
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
  result$.error_message <- error_msgs

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
  result <- dplyr::relocate(result, c(.error, .error_message), .before = dplyr::all_of(relocate_col))

  return(result)
}


oai_embed_df <- function(df, text_var, id_var, model = "text-embedding-3-small", dimensions = NULL, batch_size = 1, concurrent_requests = 1, max_retries = 5, timeout = 20, endpoint_url = "https://api.openai.com/v1/embeddings", key_name = "OPENAI_API_KEY" ) {

}



#' Generate embeddings for texts in a data frame using OpenAI
#'
#' @description
#' High-level function to generate embeddings for texts in a data frame using
#' OpenAI's embedding API. This function handles the entire process from request
#' creation to response processing, with options for batching & concurrent requests.
#'
#' @details
#' This function extracts texts from a specified column, generates embeddings using
#' `oai_embed_batch()`, and joins the results back to the original data frame using
#' a specified ID column.
#'
#' The function preserves the original data frame structure and adds new columns
#' for embedding dimensions (V1, V2, ..., Vn). If the number of rows doesn't match
#' after processing (due to errors), it returns the results with a warning.
#'
#' OpenAI's embedding API allows you to specify the number of dimensions for the
#' output embeddings, which can be useful for reducing memory usage, storage cost,s or matching
#' specific downstream requirements. The default is model-specific (1536 for
#' text-embedding-3-small). \href{https://openai.com/index/new-embedding-models-and-api-updates/}{OpenAI Embedding Updates}
#'
#' @param df Data frame containing texts to embed
#' @param text_var Column name (unquoted) containing texts to embed
#' @param id_var Column name (unquoted) for unique row identifiers
#' @param model OpenAI embedding model to use (default: "text-embedding-3-small")
#' @param dimensions Number of embedding dimensions (NULL uses model default)
#' @param key_name Name of environment variable containing the API key
#' @param batch_size Number of texts to process in one batch (default: 10)
#' @param concurrent_requests Number of concurrent requests (default: 1)
#' @param max_retries Maximum retry attempts per request (default: 5)
#' @param timeout Request timeout in seconds (default: 20)
#' @param endpoint_url OpenAI API endpoint URL
#' @param progress Whether to display a progress bar (default: TRUE)
#'
#' @return Original data frame with additional columns for embeddings (V1, V2, etc.),
#'   plus .error and .error_message columns indicating any failures
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
#'   # Generate embeddings with default dimensions
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
                         dimensions = NULL,
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

# helper funcs (may move to utils or something) ----
parse_oai_date <- function(date_string) {
  parsed_date <- as.POSIXct(date_string, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  date <- as.Date(parsed_date)
  return(date)
}

