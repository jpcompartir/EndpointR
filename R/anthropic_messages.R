# constants ----
.ANT_API_VERSION <- "2023-06-01"
.ANT_STRUCTURED_OUTPUTS_BETA <- "structured-outputs-2025-11-13"
.ANT_MESSAGES_ENDPOINT <- "https://api.anthropic.com/v1/messages"
.ANT_DEFAULT_MODEL <- "claude-haiku-4-5"

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


  return(
    list(
      messages = messages,
      body = body
    )
  )
}
