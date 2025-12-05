# constants ----
.ANT_API_VERSION <- "2023-06-01"
.ANT_STRUCTURED_OUTPUTS_BETA <- "structured-outputs-2025-11-13"
.ANT_MESSAGES_ENDPOINT <- "https://api.anthropic.com/v1/messages"
.ANT_DEFAULT_MODEL <- "claude-haiku-4-5"

# ant_build_messages_request ----
#' Build an Anthropic Messages API request
#'
#' @description
#' Constructs an httr2 request object for Anthropic's Messages API.
#' Handles message formatting, system prompts, and optional JSON schema
#' for structured outputs. When using structured outputs you must select the correct model.
#'
#'
#' @details
#' This function creates the HTTP request but does not execute it. For
#' structured outputs, you must use a supported model (Claude Sonnet 4.5
#' or Opus 4.1) and the request will automatically include the required
#' beta header.
#'
#' The `schema` parameter accepts either:
#' - A `json_schema` S7 object created with `create_json_schema()`
#' - A raw list in Anthropic's `output_format` structure
#'
#' Unlike OpenAI, Anthropic uses `output_format` (not `response_format`)
#' and the schema structure differs slightly.
#'
#' @param input Text input to send to the model
#' @param endpointr_id An id that will persist through to response
#' @param model Anthropic model to use (default: "claude-haiku-4.5")
#' @param temperature Sampling temperature (0-2), higher values = more randomness
#' @param max_tokens Maximum tokens in response
#' @param schema Optional JSON schema for structured output (json_schema object or list)
#' @param system_prompt Optional system prompt
#' @param key_name Environment variable name for API key
#' @param endpoint_url Anthropic API endpoint URL
#' @param timeout Request timeout in seconds
#' @param max_retries Maximum number of retry attempts for failed requests
#'
#' @return An httr2 request object
#' @export
#'
#' @seealso \url{https://platform.claude.com/docs/en/build-with-claude/structured-outputs}
#' @examples
#' \dontrun{
#'   # simple request
#'   req <- ant_build_messages_request(
#'     input = "What is the capital of France?",
#'     max_tokens = 100
#'   )
#'
#'   # with structured output
#'   schema <- create_json_schema(
#'     name = "capital_response",
#'     schema = schema_object(
#'       country = schema_string(),
#'       capital = schema_string(),
#'       required = c("country", "capital")
#'     )
#'   )
#'   req <- ant_build_messages_request(
#'     input = "What is the capital of France?",
#'     schema = schema,
#'     max_tokens = 100,
#'     model = "sonnet-4-5"
#'   )
#' }
ant_build_messages_request <- function(
  input,
  endpointr_id = NULL,
  model = .ANT_DEFAULT_MODEL,
  temperature = 0,
  max_tokens = 500L,
  schema = NULL,
  system_prompt = NULL,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_MESSAGES_ENDPOINT,
  timeout = 30L,
  max_retries = 5L
  ) {
  # can't use `base_request()` from core.R because Anthropic use different auth (x-api-key) so we add as a header

  stopifnot(
    "input must be a non-empty character string" = is.character(input) && length(input) == 1 && nchar(input) > 0,
    "model must be a character string" = is.character(model) && length(model) == 1,
    "temperature must be numeric between 0 and 1" = is.numeric(temperature) && temperature >= 0 && temperature <= 1, # diff to OAI API
    "max_tokens must be a positive integer" = is.numeric(max_tokens) && max_tokens > 0)

  use_structured_outputs <- FALSE  # flag for later control flow

  api_key <- get_api_key(key_name)

  messages <- list(
    list(role = "user", content = input)
  )

  body <- list(
    model = model,
    messages = messages,
    max_tokens = as.integer(max_tokens),
    temperature = temperature
  )

  # Anthropic API takes system_prompt as its own parameter, different to OAI where we concatenate

  if(!is.null(system_prompt)){
    if (!rlang::is_scalar_character(system_prompt)){
      cli::cli_abort("{.arg system_prompt} must be a {.cls character} of length 1, e.g. 'This is a valid system prompt'")
    }

    body$system <- system_prompt
  }

  #
  if(!is.null(schema)) {
    use_structured_outputs <- TRUE
    if (inherits(schema, "EndpointR::json_schema")) {
      body$output_format <- .ant_format_schema(schema)
    } else if (is.list(schema)) {
      body$output_format <- schema
    } else {
      cli::cli_abort("{.arg chema} must be an EndpointR json_schema object or a list")
    }
  }

  # build the request with headers, auth, timeout, retries, backoff (incl. system prompt if applicable)
  request <- httr2::request(endpoint_url) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "x-api-key" = api_key,
      "anthropic-version" = .ANT_API_VERSION
    ) |>
    httr2::req_error(is_error = ~ FALSE) |> # don't let httr2 auto-throw errors; we handle them ourselves
    httr2::req_timeout(timeout) |>
    httr2::req_retry(
      max_tries = max_retries,
      backoff = ~ 2 ^ .x,
      retry_on_failure = TRUE
    ) |>
    httr2::req_body_json(body)

  # if we did use structured outputs then we need to add the anthropic-beta header (this will be patched at some point I expect)

  if (use_structured_outputs) {
    request <- httr2::req_headers(request, "anthropic-beta" = .ANT_STRUCTURED_OUTPUTS_BETA)
  }

  if (!is.null(endpointr_id)) {
    request <- httr2::req_headers(request, endpointr_id = endpointr_id)
  }

  return(request)
}
# ant_build_messages_request ----


# ant_complete_text ----
#' Generate a completion for a single text using Anthropic's Messages API
#'
#' @description
#' High-level function to generate a completion for a single text string.
#' Handles request creation, execution, and response processing with
#' optional structured output support.
#'
#' @param text Character string to send to the model
#' @param model Anthropic model to use (default: "claude-sonnet-4-5-20250929")
#' @param system_prompt Optional system prompt
#' @param schema Optional JSON schema for structured output
#' @param temperature Sampling temperature (0-1)
#' @param max_tokens Maximum tokens in response
#' @param key_name Environment variable name for API key
#' @param endpoint_url Anthropic API endpoint URL
#' @param max_retries Maximum retry attempts
#' @param timeout Request timeout in seconds
#' @param tidy Whether to parse structured output (default: TRUE)
#'
#' @return Character string with the model's response, or parsed JSON if schema provided
#' @export
#'
#' @examples
#' \dontrun{
#'   # simple completion
#'   response <- ant_complete_text(
#'     text = "Explain quantum computing in simple terms",
#'     max_tokens = 500
#'   )
#'
#'   # with structured output
#'   sentiment_schema <- create_json_schema(
#'     name = "sentiment",
#'     schema = schema_object(
#'       sentiment = schema_enum(c("positive", "negative", "neutral")),
#'       confidence = schema_number(minimum = 0, maximum = 1),
#'       required = c("sentiment", "confidence")
#'     )
#'   )
#'   result <- ant_complete_text(
#'     text = "I love this product!",
#'     schema = sentiment_schema
#'   )
#' }

ant_complete_text <- function(text,
                              model = .ANT_DEFAULT_MODEL,
                              system_prompt = NULL,
                              schema = NULL,
                              temperature = 0,
                              max_tokens = 500L,
                              key_name = "ANTHROPIC_API_KEY",
                              endpoint_url = .ANT_MESSAGES_ENDPOINT,
                              max_retries = 5L,
                              timeout = 30L,
                              tidy = TRUE) {

  # surface errors quickly here, before building any request
  if (!rlang::is_scalar_character(text)) {
    cli::cli_abort(
      "{.arg text} must be a single string (a character vector of length 1)."
    )
  }

  if (nchar(text) == 0) {
    cli::cli_abort(
      "{.arg text} must not be an empty string."
    )
  }

  req <- ant_build_messages_request(
    input = text,
    model = model,
    schema = schema,
    temperature = temperature,
    max_tokens = max_tokens,
    endpoint_url = endpoint_url,
    max_retries = max_retries,
    timeout = timeout
  )

  tryCatch({
    response <- httr2::req_perform(req)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to generate completion",
      "i" = "Text: {cli::cli_vec(text, list('vec-trunc' = 50, 'vec-sep' = ''))}",
      "x" = "Error: {conditionMessage(e)}"
    ))
  })

  if (httr2::resp_status(response) != 200) {
    error_msg <- .extract_api_error(response)
    cli::cli_abort(c(
      "API request failed",
      "x" = error_msg
    ))
  }

  content <- .extract_ant_message_content(response)

  if (!is.null(schema) && tidy && !is.na(content)) {
    content <- tryCatch({
      parsed <- jsonlite::fromJSON(content, simplifyVector = FALSE)
      if (!is.null(schema)) {
        parsed <- validate_response(schema, content)
      }
      parsed
    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to parse structured output",
        "i" = "Returning raw response",
        "x" = conditionMessage(e)
      ))
      content
    })
  }

  return(content)
}
# ant_complete_text ----

# ant_complete_chunks ----
#' Process text chunks through Anthropic's Messages API with batch file output
#'
#' @description
#' Processes large volumes of text through Anthropic's Messages API in
#' configurable chunks, writing results progressively to parquet files.
#' Handles concurrent requests, automatic retries, and structured outputs.
#'
#' @details
#' This function is designed for processing large text datasets. It divides
#' input into chunks, processes each chunk with concurrent API requests, and
#' writes results to disk to minimise memory usage and possibility of data loss.
#'
#' Results are written as parquet files in the specified output directory,
#' along with a metadata.json file containing processing parameters.
#'
#' When using the `output_dir =` argument, be careful that you select
#' a new directory if you do not wish to overwrite existing chunks.
#' If there is already a `chunks_001.parquet` file in the directory,
#' it will be overwritten.
#'
#' @param texts Character vector of texts to process
#' @param ids Vector of unique identifiers (same length as texts)
#' @param chunk_size Number of texts per chunk before writing to disk
#' @param model Anthropic model to use
#' @param system_prompt Optional system prompt (applied to all requests)
#' @param output_dir Directory for parquet chunks ("auto" generates timestamped dir)
#' @param schema Optional JSON schema for structured output
#' @param concurrent_requests Number of concurrent requests
#' @param temperature Sampling temperature
#' @param max_tokens Maximum tokens per response
#' @param max_retries Maximum retry attempts per request
#' @param timeout Request timeout in seconds
#' @param key_name Environment variable name for API key
#' @param endpoint_url Anthropic API endpoint URL
#' @param id_col_name Name for ID column in output
#'
#' @return A tibble with all results
#' @export
ant_complete_chunks <- function(texts,
                                ids,
                                chunk_size = 5000L,
                                model = "claude-haiku-4-5",
                                system_prompt = NULL,
                                output_dir = "auto",
                                schema = NULL,
                                concurrent_requests = 5L,
                                temperature = 0,
                                max_tokens = 1024L,
                                max_retries = 5L,
                                timeout = 30L,
                                key_name = "ANTHROPIC_API_KEY",
                                endpoint_url = .ANT_MESSAGES_ENDPOINT,
                                id_col_name = "id") {

  stopifnot(
    "texts must be a vector" = is.vector(texts),
    "ids must be a vector" = is.vector(ids),
    "texts and ids must be the same length" = length(texts) == length(ids),
    "chunk_size must be a positive integer" = is.numeric(chunk_size) && chunk_size > 0
  )

  output_dir <- .handle_output_directory(output_dir, base_dir_name = "ant_messages_chunks")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # build the schema just once (and apply later)
  if (!is.null(schema) && inherits(schema, "EndpointR::json_schema")) {
    formatted_schema <- .ant_format_schema(schema)
  } else {
    formatted_schema <- schema
  }

  chunk_data <- batch_vector(seq_along(texts), chunk_size)
  n_chunks <- length(chunk_data$batch_indices)

  metadata <- list(
    output_dir = output_dir,
    endpoint_url = endpoint_url,
    model = model,
    schema = NULL,
    has_system_prompt = !is.null(system_prompt),
    chunk_size = chunk_size,
    n_chunks = n_chunks,
    n_texts = length(texts),
    concurrent_requests = concurrent_requests,
    timeout = timeout,
    max_retries = max_retries,
    max_tokens = max_tokens,
    temperature = temperature,
    key_name = key_name,
    timestamp = Sys.time()
  )

  if (!is.null(formatted_schema)) {
    metadata$schema <- jsonlite::toJSON(formatted_schema) |>
      jsonlite::prettify()
  }

  jsonlite::write_json(
    metadata,
    file.path(output_dir, "metadata.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  cli::cli_alert_info("Processing {length(texts)} text{?s} in {n_chunks} chunk{?s} of up to {chunk_size} each")
  cli::cli_alert_info("Results will be saved as parquet files in {output_dir}")

  total_successes <- 0
  total_failures <- 0

  # core processing logic
  for (chunk_num in seq_along(chunk_data$batch_indices)) {
    chunk_indices <- chunk_data$batch_indices[[chunk_num]]
    chunk_texts <- texts[chunk_indices]
    chunk_ids <- ids[chunk_indices]

    cli::cli_progress_message("Processing chunk {chunk_num}/{n_chunks} ({length(chunk_indices)} text{?s})")

    # within chunk reqs
    requests <- purrr::map2(
      .x = chunk_texts,
      .y = chunk_ids,
      .f = \(x, y) ant_build_messages_request(
        input = x,
        endpointr_id = y,
        model = model,
        temperature = temperature,
        max_tokens = max_tokens,
        schema = formatted_schema,
        system_prompt = system_prompt,
        key_name = key_name,
        endpoint_url = endpoint_url,
        max_retries = max_retries,
        timeout = timeout
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

    is_response <- purrr::map_lgl(responses, inherits, "httr2_response")
    response_objects <- responses[is_response]
    error_objects <- responses[!is_response]

    is_success <- purrr::map_lgl(response_objects, \(x) httr2::resp_status(x) < 400)
    successes <- response_objects[is_success]
    http_failures <- response_objects[!is_success]

    failures <- c(http_failures, error_objects)

    n_successes <- length(successes)
    n_failures <- length(failures)
    total_successes <- total_successes + n_successes
    total_failures <- total_failures + n_failures


    chunk_results <- list()

    if (n_successes > 0) {
      successes_ids <- purrr::map(successes, \(x) purrr::pluck(x, "request", "headers", "endpointr_id")) |> unlist()
      successes_content <- purrr::map_chr(successes, .extract_ant_message_content)

      chunk_results$successes <- tibble::tibble(
        !!id_col_name := successes_ids,
        content = successes_content,
        .error = FALSE,
        .error_msg = NA_character_,
        .status = NA_integer_,
        .chunk = chunk_num
      )
    }

    if (n_failures > 0) {
      failures_ids <- purrr::map(failures, \(x) purrr::pluck(x, "request", "headers", "endpointr_id")) |> unlist()
      failures_msgs <- purrr::map_chr(failures, \(x){
        if (inherits(x, "httr2_response")) {
          .extract_api_error(x)
        } else {
          # error object - try to get resp from it
          resp <- purrr::pluck(x, "resp")
          if (!is.null(resp)) .extract_api_error(resp) else .extract_api_error(x, "Unknown error")
        }
      })
      failures_status <- purrr::map_int(failures, \(x){
        if (inherits(x, "httr2_response")) {
          httr2::resp_status(x)
        } else {
          resp <- purrr::pluck(x, "resp")
          if (!is.null(resp)) httr2::resp_status(resp) else NA_integer_
        }
      })

      chunk_results$failures <- tibble::tibble(
        !!id_col_name := failures_ids,
        content = NA_character_,
        .error = TRUE,
        .error_msg = failures_msgs,
        .status = failures_status,
        .chunk = chunk_num
      )
    }

    chunk_df <- dplyr::bind_rows(chunk_results)

    if (nrow(chunk_df) > 0) {
      chunk_file <- glue::glue("{output_dir}/chunk_{stringr::str_pad(chunk_num, 3, pad = '0')}.parquet")
      arrow::write_parquet(chunk_df, chunk_file)
    }

    cli::cli_alert_success("Chunk {chunk_num}: {n_successes} successful, {n_failures} failed")

    rm(requests, responses, successes, failures, chunk_results, chunk_df)
    gc(verbose = FALSE)
  }

  cli::cli_alert_info("Processing completed, there were {total_successes} successes\n and {total_failures} failures.")

  parquet_files <- list.files(output_dir, pattern = "\\.parquet$", full.names = TRUE)
  final_results <- arrow::open_dataset(parquet_files, format = "parquet") |>
    dplyr::collect()

  return(final_results)
}
# ant_complete_chunks ----

# ant_complete_df ----
ant_complete_df <- function(df,
                            text_var,
                            id_var,
                            model = "claude-haiku-4-5",
                            output_dir = "auto",
                            system_prompt = NULL,
                            schema = NULL,
                            chunk_size = 5000L,
                            concurrent_requests = 5L,
                            max_retries = 5L,
                            timeout = 30,
                            temperature = 0,
                            max_tokens = 1024L,
                            key_name = "ANTHROPIC_API_KEY",
                            endpoint_url = .ANT_MESSAGES_ENDPOINT) {

  text_sym <- rlang::ensym(text_var)
  id_sym <- rlang::ensym(id_var)

  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "df must not be empty" = nrow(df) > 0,
    "text_var must exist in df" = rlang::as_name(text_sym) %in% names(df),
    "id_var must exist in df" = rlang::as_name(id_sym) %in% names(df),
    "model must be a character vector" = is.character(model),
    "`chunk_size` must be a positive integer" = is.numeric(chunk_size) && chunk_size > 0
  )

  output_dir <- .handle_output_directory(output_dir, base_dir_name = "ant_messages_chunks")

  text_vec <- dplyr::pull(df, !!text_sym)
  id_vec <- dplyr::pull(df, !!id_sym)


  id_col_name <- rlang::as_name(id_sym) # needed for preserving original col names in chunks func (which doesn't receive the id_var, but a vec of ids+texts)

  results <- ant_complete_chunks(
    texts = text_vec,
    ids = id_vec,
    model = model,
    system_prompt = system_prompt,
    schema = schema,
    chunk_size = chunk_size,
    concurrent_requests = concurrent_requests,
    max_retries = max_retries,
    timeout = timeout,
    temperature = temperature,
    max_tokens = max_tokens,
    key_name = key_name,
    endpoint_url = endpoint_url,
    output_dir = output_dir,
    id_col_name = id_col_name
  )

  results <- dplyr::rename(results, !!id_sym := !!rlang::sym(id_col_name))

  return(results)
}

# ant_complete_df ----

#' Extract text content from Anthropic Messages API response
#' @keywords internal
.extract_ant_message_content <- function(resp) {
  body <- httr2::resp_body_json(resp)

  # find the first text block
  content_blocks <- body$content

  if (length(content_blocks) == 0) {
    return(NA_character_)
  }

  for (block in content_blocks) {
    if (block$type == "text") {
      return(block$text)
    }
  }
  return(NA_character_)
}


#' Convert json_schema S7 object to Anthropic output_format structure
#' @keywords internal
.ant_format_schema <- function(schema) {
  if (!inherits(schema, "EndpointR::json_schema")) {
    cli::cli_abort("schema must be a json_schema object")
  }

  # Anthropic uses output_format with type "json_schema"
  # The schema goes directly in the "schema" field (not nested like OpenAI)
  list(
    type = "json_schema",
    schema = schema@schema
  )
}

