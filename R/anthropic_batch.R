# constants ----
.ANT_BATCHES_ENDPOINT <- "https://api.anthropic.com/v1/messages/batches"

# ant_batch_create ----
#' Create an Anthropic Message Batch
#'
#' @description
#' Submits a batch of message requests to Anthropic's Message Batches API.
#' Batches are processed asynchronously with 50% cost savings and a 24-hour
#' completion window.
#'
#' @details
#' Each request in the batch is a standalone Messages API call. The API
#' supports up to 100,000 requests per batch.
#'
#' Each request is built by the same code as [ant_build_messages_request()],
#' so a batch request and a Messages request with the same arguments send the
#' same fields.
#'
#' When `system_prompt` is provided, prompt caching is automatically enabled
#' by adding `cache_control` to the system prompt.
#'
#' For structured outputs, pass a `json_schema` object to `schema`. This uses
#' the GA `output_config` format (no beta header required).
#'
#' The API checks each request only after the whole batch has run, so a bad
#' setting is accepted when you submit the batch and only shows up as an
#' error in [ant_batch_results()], up to 24 hours later. EndpointR checks the
#' `effort` and `thinking` rules it knows about before submitting, but test
#' one request with [ant_complete_text()] and the same settings before you
#' submit a large batch.
#'
#' @param texts Character vector of texts to send to the model
#' @param custom_ids Character vector of unique identifiers (same length as texts)
#' @param system_prompt Optional system prompt (applied to all requests)
#' @param schema Optional JSON schema for structured output (json_schema object or list)
#' @param max_tokens Maximum tokens per response, including thinking tokens
#' @param key_name Environment variable name for API key
#' @param endpoint_url Anthropic Batches API endpoint URL
#' @param timeout Request timeout in seconds
#' @inheritParams ant_build_messages_request
#' @inheritSection ant_build_messages_request Effort and thinking
#'
#' @return A list of batch metadata including `$id` for tracking
#' @export
#'
#' @seealso [ant_batch_status()], [ant_batch_results()], [ant_batch_list()], [ant_batch_cancel()]
#' @examples
#' \dontrun{
#'   batch <- ant_batch_create(
#'     texts = c("Hello", "World"),
#'     custom_ids = c("t1", "t2"),
#'     system_prompt = "Reply in one word",
#'     effort = "low"
#'   )
#'   # Check status later
#'   ant_batch_status(batch$id)
#' }
ant_batch_create <- function(
  texts,
  custom_ids,
  model = .ANT_DEFAULT_MODEL,
  system_prompt = NULL,
  schema = NULL,
  max_tokens = 4096L,
  temperature = 0,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_BATCHES_ENDPOINT,
  timeout = 60L,
  effort = NULL,
  thinking = NULL
) {

  stopifnot(
    "texts must be a character vector" = is.character(texts) && length(texts) > 0,
    "custom_ids must be a character vector" = is.character(custom_ids) && length(custom_ids) > 0,
    "texts and custom_ids must be the same length" = length(texts) == length(custom_ids),
    "custom_ids must be unique" = !anyDuplicated(custom_ids),
    "batch cannot exceed 100,000 requests" = length(texts) <= 100000
  )

  bad_ids <- !grepl("^[a-zA-Z0-9_-]{1,64}$", custom_ids)
  if (any(bad_ids)) {
    n_bad <- sum(bad_ids)
    examples <- utils::head(custom_ids[bad_ids], 3)
    cli::cli_abort(c(
      "{.arg custom_ids} must be 1-64 characters using only letters, numbers, hyphens, and underscores",
      "i" = "{n_bad} id{?s} {?does/do} not meet this requirement",
      "i" = "Example{?s}: {.val {examples}}"
    ))
  }

  # build the params once with the same code as the Messages route, which also
  # checks the arguments, then swap in each text
  params <- .ant_build_body(
    input = texts[[1]],
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    system_prompt = system_prompt,
    schema = schema,
    effort = effort,
    thinking = thinking
  )

  requests <- purrr::map2(texts, custom_ids, function(text, id) {
    params$messages[[1]]$content <- text
    list(
      custom_id = id,
      params = params
    )
  })

  api_key <- get_api_key(key_name)

  body <- list(requests = requests)

  response <- httr2::request(endpoint_url) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "x-api-key" = api_key,
      "anthropic-version" = .ANT_API_VERSION
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_timeout(timeout) |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 400) {
    error_msg <- .extract_api_error(response)
    cli::cli_abort(c(
      "Batch creation failed",
      "x" = "{error_msg}"
    ))
  }

  httr2::resp_body_json(response)
}

# ant_batch_status ----
#' Check the Status of an Anthropic Message Batch
#'
#' @description
#' Retrieves the current status and metadata for a message batch.
#'
#' @param batch_id Character string of the batch ID (returned by `ant_batch_create()`)
#' @param key_name Environment variable name for API key
#' @param endpoint_url Anthropic Batches API endpoint URL
#'
#' @return A list of batch metadata including `processing_status`, `request_counts`, etc.
#' @export
#'
#' @examples
#' \dontrun{
#'   status <- ant_batch_status("msgbatch_abc123")
#'   status$processing_status # e.g. "in_progress", "ended"
#' }
ant_batch_status <- function(
  batch_id,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_BATCHES_ENDPOINT
) {

  stopifnot(
    "batch_id must be a non-empty character string" = is.character(batch_id) && length(batch_id) == 1 && nchar(batch_id) > 0
  )

  api_key <- get_api_key(key_name)

  response <- httr2::request(paste0(endpoint_url, "/", batch_id)) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = .ANT_API_VERSION
    ) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 400) {
    error_msg <- .extract_api_error(response)
    cli::cli_abort(c(
      "Failed to retrieve batch status",
      "x" = "{error_msg}"
    ))
  }

  httr2::resp_body_json(response)
}

# ant_batch_results ----
#' Retrieve Results from a Completed Anthropic Message Batch
#'
#' @description
#' Downloads and parses results from a completed message batch. The batch
#' must have `processing_status` of `"ended"` before results can be retrieved.
#'
#' @details
#' Results are returned as a tibble with one row per request. The function
#' handles all four Anthropic result types: succeeded, errored, canceled,
#' and expired.
#'
#' `thinking_tokens` is `NA` when the API doesn't report it. A warning is
#' shown when any request ran out of tokens before writing an answer.
#'
#' @inheritParams ant_batch_status
#'
#' @return A tibble with columns: `custom_id`, `content`, `.error`,
#'   `.error_msg`, `stop_reason`, `input_tokens`, `output_tokens`,
#'   `thinking_tokens`
#' @export
#'
#' @examples
#' \dontrun{
#'   results <- ant_batch_results("msgbatch_abc123")
#'   results$content # model responses
#' }
ant_batch_results <- function(
  batch_id,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_BATCHES_ENDPOINT
) {

  batch_meta <- ant_batch_status(
    batch_id,
    key_name = key_name,
    endpoint_url = endpoint_url
  )

  if (batch_meta$processing_status != "ended") {
    cli::cli_abort(c(
      "Batch is not yet complete",
      "i" = "Current status: {batch_meta$processing_status}",
      "i" = "Use {.fn ant_batch_status} to check progress"
    ))
  }

  results_url <- batch_meta$results_url

  if (is.null(results_url)) {
    cli::cli_abort("Batch has ended but no results URL is available")
  }

  api_key <- get_api_key(key_name)

  response <- httr2::request(results_url) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = .ANT_API_VERSION
    ) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 400) {
    error_msg <- .extract_api_error(response)
    cli::cli_abort(c(
      "Failed to retrieve batch results",
      "x" = "{error_msg}"
    ))
  }

  results <- .ant_parse_batch_results(httr2::resp_body_string(response))
  .ant_warn_out_of_tokens(results$content, results$stop_reason)

  results
}

# ant_batch_list ----
#' List Anthropic Message Batches
#'
#' @description
#' Retrieves a paginated list of message batches.
#'
#' @param limit Maximum number of batches to return (1-1000, default 20)
#' @param before_id Cursor for backward pagination (batch ID)
#' @param after_id Cursor for forward pagination (batch ID)
#' @param key_name Environment variable name for API key
#' @param endpoint_url Anthropic Batches API endpoint URL
#'
#' @return A list containing batch metadata and pagination information
#' @export
#'
#' @examples
#' \dontrun{
#'   batches <- ant_batch_list(limit = 10)
#' }
ant_batch_list <- function(
  limit = 20L,
  before_id = NULL,
  after_id = NULL,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_BATCHES_ENDPOINT
) {

  stopifnot(
    "limit must be between 1 and 1000" = is.numeric(limit) && limit >= 1 && limit <= 1000
  )

  api_key <- get_api_key(key_name)

  req <- httr2::request(endpoint_url) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = .ANT_API_VERSION
    ) |>
    httr2::req_url_query(limit = as.integer(limit)) |>
    httr2::req_error(is_error = ~ FALSE)

  if (!is.null(before_id)) {
    req <- httr2::req_url_query(req, before_id = before_id)
  }
  if (!is.null(after_id)) {
    req <- httr2::req_url_query(req, after_id = after_id)
  }

  response <- httr2::req_perform(req)

  if (httr2::resp_status(response) >= 400) {
    error_msg <- .extract_api_error(response)
    cli::cli_abort(c(
      "Failed to list batches",
      "x" = "{error_msg}"
    ))
  }

  httr2::resp_body_json(response)
}

# ant_batch_cancel ----
#' Cancel an Anthropic Message Batch
#'
#' @description
#' Cancels an in-progress message batch. Requests already being processed
#' may still complete.
#'
#' @inheritParams ant_batch_status
#'
#' @return A list of batch metadata reflecting the cancellation
#' @export
#'
#' @examples
#' \dontrun{
#'   ant_batch_cancel("msgbatch_abc123")
#' }
ant_batch_cancel <- function(
  batch_id,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_BATCHES_ENDPOINT
) {

  stopifnot(
    "batch_id must be a non-empty character string" = is.character(batch_id) && length(batch_id) == 1 && nchar(batch_id) > 0
  )

  api_key <- get_api_key(key_name)

  response <- httr2::request(paste0(endpoint_url, "/", batch_id, "/cancel")) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = .ANT_API_VERSION
    ) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 400) {
    error_msg <- .extract_api_error(response)
    cli::cli_abort(c(
      "Failed to cancel batch",
      "x" = "{error_msg}"
    ))
  }

  httr2::resp_body_json(response)
}

# internal helpers ----
#' Extract text content from a batch result message object
#' @keywords internal
.extract_content_from_batch_message <- function(msg) {
  content_blocks <- msg$content

  if (is.null(content_blocks) || length(content_blocks) == 0) {
    return(NA_character_)
  }

  for (block in content_blocks) {
    if (block$type == "text") {
      return(block$text)
    }
  }
  return(NA_character_)
}

#' Parse the JSONL body of a batch's results into a tibble
#' @param raw_text The results file as one string
#' @return A tibble with one row per line
#' @keywords internal
.ant_parse_batch_results <- function(raw_text) {
  lines <- strsplit(raw_text, "\n")[[1]]
  lines <- lines[nchar(lines) > 0]

  empty_results <- tibble::tibble(
    custom_id = character(),
    content = character(),
    .error = logical(),
    .error_msg = character(),
    stop_reason = character(),
    input_tokens = integer(),
    output_tokens = integer(),
    thinking_tokens = integer()
  )

  if (length(lines) == 0) {
    return(empty_results)
  }

  parsed <- purrr::imap(lines, \(line, idx) {
    tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) {
        list(
          custom_id = paste0("__PARSE_ERROR_LINE_", idx),
          result = list(type = "errored", error = list(message = paste("Failed to parse JSONL line", idx)))
        )
      }
    )
  })

  results <- purrr::map(parsed, function(item) {
    result <- item$result

    if (result$type == "succeeded") {
      fields <- .ant_message_fields(result$message)
      return(tibble::tibble(
        custom_id = item$custom_id,
        content = fields$content,
        .error = FALSE,
        .error_msg = NA_character_,
        stop_reason = fields$stop_reason,
        input_tokens = fields$input_tokens,
        output_tokens = fields$output_tokens,
        thinking_tokens = fields$thinking_tokens
      ))
    }

    # errored, canceled, expired. The API nests an errored request's message
    # one level down, in result$error$error$message
    error_msg <- if (result$type == "errored") {
      result$error$error$message %||% result$error$message %||% "Unknown error"
    } else {
      result$type
    }

    tibble::tibble(
      custom_id = item$custom_id,
      content = NA_character_,
      .error = TRUE,
      .error_msg = error_msg,
      stop_reason = NA_character_,
      input_tokens = NA_integer_,
      output_tokens = NA_integer_,
      thinking_tokens = NA_integer_
    )
  })

  purrr::list_rbind(results)
}
