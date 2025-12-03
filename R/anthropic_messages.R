# constants ----
.ANT_API_VERSION <- "2023-06-01"
.ANT_STRUCTURED_OUTPUTS_BETA <- "structured-outputs-2025-11-13"
.ANT_MESSAGES_ENDPOINT <- "https://api.anthropic.com/v1/messages"
.ANT_DEFAULT_MODEL <- "claude-haiku-4-5"

#' Build an Anthropic Messages API request
#'
#' @description
#' Constructs an httr2 request object for Anthropic's Messages API.
#' Handles message formatting, system prompts, and optional JSON schema
#' for structured outputs.
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
#'     max_tokens = 100
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
  }

  #
  if(!is.null(schema)) {
    use_structured_outputs <- TRUE
    if (inherits(schema, "EndpointR::json_schema")) {
      body$output_format <- .ant_format_schema(schema)
    } else if (is.list(schema)) {
      cli::cli_alert_warning("Your {.arg schema} is a list, not an EndpointR json_schema")
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

