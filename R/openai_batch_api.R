# the batch
oai_batch_build_embed_req <- function(input, id, model = "text-embedding-3-small", dimensions = NULL, method = "POST", encoding_format = "float", endpoint = "/v1/embeddings") {


  body <- purrr::compact(
    # use compact so that if dimensions is NULL it gets dropped from the req
    list(
      input = input,
      model = model,
      dimensions = dimensions,
      encoding_format = encoding_format
  ))

  embed_row <- list(
    custom_id = id,
    method = method,
    url = endpoint,
    body = body
  )

  embed_row_json <- jsonlite::toJSON(embed_row,
                                     auto_unbox = TRUE)

  return(embed_row_json)
}

oai_batch_prepare_embeddings <- function(df, text_var, id_var, model = "text-embedding-3-small", dimensions = NULL, method = "POST", encoding_format = "float", endpoint = "/v1/embeddings") {

  text_sym <- rlang::ensym(text_var)
  id_sym <- rlang::ensym(id_var)

  .texts <- dplyr::pull(df, !!text_sym)
  .ids <- dplyr::pull(df, !!id_sym)

  if (!.validate_batch_inputs(.ids, .texts)) {
    return("")
  }

  reqs <- purrr::map2_chr(.texts, .ids, \(x, y) {
    oai_batch_build_embed_req(
      input = x,
      id = as.character(y),
      model = model,
      dimensions = dimensions,
      method = method,
      encoding_format = encoding_format,
      endpoint = endpoint
    )
  })

  reqs <- paste0(reqs, collapse = "\n")

  return(reqs)
}

oai_batch_file_upload <- function(jsonl_rows, key_name = "OPENAI_API_KEY", purpose = "batch") {

  api_key <- get_api_key(key_name)

  .tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(.tmp)) # if session crashes we drop the file from mem safely
  writeLines(jsonl_rows, .tmp) # send the content to the temp file for uploading to OAI
  # question here is whether to also save this somewhere by force...
  # once OAI have the file it's backed up for 30 days.

 resp <- httr2::request(base_url = "https://api.openai.com/v1/files") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_multipart(file = curl::form_file(.tmp),
                              purpose = purpose) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)

  if (httr2::resp_status(resp) >= 400) {
    error_msg <- result$error$message %||% "Unknown error"
    cli::cli_abort(c(
      "Failed to upload file to OpenAI Files API",
      "x" = error_msg
    ))
  }

  return(result)
}

# batch job management ----
oai_batch_create <- function(file_id,
                              endpoint = c("/v1/embeddings", "/v1/chat/completions"),
                              completion_window = "24h",
                              metadata = NULL,
                              key_name = "OPENAI_API_KEY") {

  endpoint <- match.arg(endpoint)
  api_key <- get_api_key(key_name)

  body <- list(
    input_file_id = file_id,
    endpoint = endpoint,
    completion_window = completion_window
  )

  if (!is.null(metadata)) {
    body$metadata <- metadata
  }

  httr2::request("https://api.openai.com/v1/batches") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
oai_batch_list <- function(limit = 20L, after = NULL, key_name = "OPENAI_API_KEY") {

  api_key <- get_api_key(key_name)

  req <- httr2::request("https://api.openai.com/v1/batches") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_url_query(limit = limit)

  if (!is.null(after)) {
    req <- httr2::req_url_query(req, after = after)
  }

  req |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

    httr2::req_perform() |>
    httr2::resp_body_json()
oai_batch_parse_embeddings <- function(content, original_df = NULL, id_var = NULL) {

  lines <- strsplit(content, "\n")[[1]]
  lines <- lines[nchar(lines) > 0]

  if (length(lines) == 0) {
    return(tibble::tibble(
      custom_id = character(),
      .error = logical(),
      .error_msg = character()
    ))
  }

  parsed <- purrr::imap(lines, \(line, idx) {
    tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) {
        list(
          custom_id = paste0("__PARSE_ERROR_LINE_", idx),
          error = list(message = paste("Failed to parse JSONL line", idx, ":", conditionMessage(e)))
        )
      }
    )
  })

  results <- purrr::map(parsed, function(item) {
    custom_id <- item$custom_id

    if (!is.null(item$error)) {
      return(tibble::tibble(
        custom_id = custom_id,
        .error = TRUE,
        .error_msg = item$error$message %||% "Unknown error"
      ))
    }

    embedding <- purrr::pluck(item, "response", "body", "data", 1, "embedding",
                               .default = NULL)

    if (is.null(embedding)) {
      return(tibble::tibble(
        custom_id = custom_id,
        .error = TRUE,
        .error_msg = "No embedding found in response"
      ))
    }

    embed_tibble <- embedding |>
      as.list() |>
      stats::setNames(paste0("V", seq_along(embedding))) |>
      tibble::as_tibble()

    tibble::tibble(
      custom_id = custom_id,
      .error = FALSE,
      .error_msg = NA_character_
    ) |>
      dplyr::bind_cols(embed_tibble)
  })

  result <- purrr::list_rbind(results)

  if (!is.null(original_df) && !is.null(id_var)) {
    id_sym <- rlang::ensym(id_var)
    id_col_name <- rlang::as_name(id_sym)
    result <- result |>
      dplyr::rename(!!id_col_name := custom_id)
  }

  return(result)
}




# internal/helper
.validate_batch_inputs <- function(.ids, .texts, max_requests = 50000) {
  n_requests <- length(.texts)

  if (n_requests == 0) {
    cli::cli_warn("Input is empty. Returning empty JSONL string.")
    return(FALSE)
  }

  if (anyDuplicated(.ids)) {
    duplicated_ids <- unique(.ids[duplicated(.ids)])
    cli::cli_abort(c(
      "custom_id values must be unique within a batch",
      "x" = "Found {length(duplicated_ids)} duplicate ID{?s}: {.val {head(duplicated_ids, 3)}}"
    ))
  }

  if (n_requests > max_requests) {
    cli::cli_abort(c(
      "OpenAI Batch API supports maximum {max_requests} requests per batch",
      "x" = "Attempting to create {n_requests} requests",
      "i" = "Consider splitting your data into multiple batches"
    ))
  }

  if (n_requests > 10000) {
    cli::cli_alert_info("Large batch with {n_requests} requests - processing may take significant time")
  }

  return(TRUE)
}
