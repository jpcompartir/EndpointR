#' Build an OpenAI API Chat Completions request
#'
#' This function constructs a httr2 request object specifically tailored for
#' interacting with OpenAI's Chat Completions API. It handles the
#' formatting of messages, model parameters, and optional JSON schema
#' for structured responses.
#'
#' @details This function simplifies the process of making calls to the OpenAI Chat
#' Completions API by assembling the request body according to the API's
#' specifications.
#'
#' The `input` and `system_prompt` (if provided) are automatically structured
#' into the required 'messages' array format for the API.
#'
#' For structured outputs (JSON mode), you will need to provide a valid JSON schema
#' via the `schema` parameter. This can be a pre-formatted list or an object of
#' class "json_schema". If a schema is provided, the function will automatically
#' set `schema$additionalProperties <- FALSE` and ensure `schema$strict <- TRUE`
#' (if `strict` is not already defined in the schema) to encourage more predictable
#' and reliable structured outputs from the API. It's also a good idea
#' to set temperature to 0 when extracting structured outputs.
#'
#' @param input Text input to send to the model
#' @param model OpenAI model to use (default: "gpt-4.1-nano")
#' @param temperature Sampling temperature (0-2), higher values = more randomness
#' @param max_tokens Maximum tokens in response
#' @param schema Optional JSON schema for structured output (json_schema object or list)
#' @param system_prompt Optional system prompt
#' @param key_name Environment variable name for API key
#' @param endpoint_url OpenAI API endpoint URL
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param timeout Request timeout in seconds
#'
#' @return An httr2 request object
#' @export
#' @seealso \href{https://platform.openai.com/docs/guides/responses-vs-chat-completions}{Completions vs Responses API}
oai_build_completions_request <- function(
    input,
    model = "gpt-4.1-nano",
    temperature = 0,
    max_tokens = 500L,
    schema = NULL,
    system_prompt = NULL,
    key_name = "OPENAI_API_KEY",
    endpoint_url = "https://api.openai.com/v1/chat/completions",
    timeout = 20,
    max_retries = 5) {

  stopifnot(
    "input must be a non-empty character string" = is.character(input) && length(input) == 1 && nchar(input) > 0,
    "model must be a character string" = is.character(model) && length(model) == 1,
    "temperature must be numeric between 0 and 2" = is.numeric(temperature) && temperature >= 0 && temperature <= 2,
    "max_tokens must be a positive integer" = is.numeric(max_tokens) && max_tokens > 0
  )

  api_key <- get_api_key(key_name)

  messages <- list() # chat completions manage the chat with a list of messages, so we append messages to this list with role and content.
  if (!is.null(system_prompt)) { # append system prompt to empty messages
    if (!is.character(system_prompt) || length(system_prompt) != 1) {
      cli::cli_abort("system_prompt must be a single character string")
    }

    messages <- append(messages,
                       list(
                         list(role = "system",
                              content = system_prompt)
                       )
    )
  }
  # if we didn't have a system prompt then this will be the first message as is required
  messages <- append(messages,
                     list(
                       list(role = "user",
                            content = input)))

  body <- list(
    model = model,
    messages = messages,
    temperature = temperature,
    max_tokens = max_tokens
  )

  if (!is.null(schema)) {
    if (inherits(schema, "EndpointR::json_schema") || inherits(schema, "json_schema") || inherits(schema, "S7_object")){
      schema_dump <- json_dump(schema)

      if (!is.null(schema_dump$json_schema$schema)) {
        schema_dump$json_schema$schema$additionalProperties <- FALSE # must be the case for OAI structured outputs
      }

      body$response_format <- schema_dump
    } else {
      body$response_format <- schema
    }
  }

  request <- base_request(endpoint_url = endpoint_url,
                          api_key = api_key) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = max_retries,
                     backoff = ~ 2 ^ .x,
                     retry_on_failure = TRUE) |>
    httr2::req_body_json(body)

  return(request)
}

#' Build OpenAI requests for batch processing
#'
#' @param inputs Character vector of text inputs
#' @param model OpenAI model to use
#' @param temperature Sampling temperature
#' @param max_tokens Maximum tokens per response
#' @param schema Optional JSON schema for structured output
#' @param system_prompt Optional system prompt
#' @param key_name Environment variable name for API key
#' @param endpoint_url OpenAI API endpoint URL
#'
#' @return List of httr2 request objects
#' @export
oai_build_completions_request_list <- function(
    inputs,
    model = "gpt-4.1-nano",
    temperature = 0,
    max_tokens = 500L,
    schema = NULL,
    system_prompt = NULL,
    max_retries = 5L,
    timeout = 30,
    key_name = "OPENAI_API_KEY",
    endpoint_url = "https://api.openai.com/v1/chat/completions") {

  stopifnot(
    "inputs must be a character vector" = is.character(inputs),
    "inputs must not be empty" = length(inputs) > 0
  )

  invalid_indices <- which(is.na(inputs) | nchar(inputs) == 0)

  if (length(invalid_indices) > 0) {
    cli::cli_abort("Inputs at indices: {invalid_indices} are empty or NA. Filter or amend before proceeding.")
  }

  requests <- purrr::map(inputs, ~oai_build_completions_request(
    input = .x,
    model = model,
    temperature = temperature,
    max_tokens = max_tokens,
    schema = schema,
    system_prompt = system_prompt,
    key_name = key_name,
    endpoint_url = endpoint_url,
    max_retries = max_retries,
    timeout = timeout
  ))

  return(requests)
}


# start of complete_df func ----
oai_complete_df <- function(df,
                            text_var,
                            id_var,
                            model = "gpt-4.1-nano",
                            system_prompt = NULL,
                            schema = NULL,
                            concurrent_requests = 1L,
                            max_retries = 5L,
                            timeout = 30,
                            temperature = 0,
                            max_tokens = 500L,
                            progress = TRUE,
                            key_name = "OPENAI_API_KEY",
                            endpoint_url = "https://api.openai.com/v1/chat/completions"
) {
  text_sym <- rlang::ensym(text_var)
  id_sym <- rlang::ensym(id_var)

  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "df must not be empty" = nrow(df) > 0,
    "text_var must exist in df" = rlang::as_name(text_sym) %in% names(df),
    "id_var must exist in df" = rlang::as_name(id_sym) %in% names(df)
  )

  # pull inputs and ids into vectors for later concurrency
  inputs <- dplyr::pull(df, !!text_sym)
  ids <- dplyr::pull(df, !!id_sym)

  requests <- oai_build_completions_request_list(
    inputs = inputs,
    model = model,
    temperature = temperature,
    max_tokens = max_tokens,
    schema = schema,
    system_prompt = system_prompt,
    key_name = key_name,
    endpoint_url = endpoint_url,
    max_retries = max_retries,
    timeout = timeout
  )

  # keep track of valid/invalid requests and IDs explicitly separately
  is_httr2_request <- purrr::map_lgl(requests, ~class(.x) == "httr2_request")
  invalid_indices <- ids[which(!is_httr2_request)]
  invalid_df <- tibble::tibble(!!id_sym := invalid_indices)

  valid_indices <- ids[which(is_httr2_request)]
  valid_df <- tibble::tibble(!!id_sym := valid_indices)

  requests <- requests[which(is_httr2_request)] # changes the shape if we have an errored request, so need to be careful later with the inputs and ids -> data frame

  responses <- perform_requests_with_strategy(
    requests,
    concurrent_requests = concurrent_requests,
    progress = progress
  )

  # browser()

  responses_df <- tibble::tibble(
    !!id_sym := valid_indices,
    response = responses
  ) |>
    dplyr::mutate(
      is_resp = purrr::map_lgl(response, ~inherits(.x, "httr2_response"))
    )

  # preferring the separate dfs for success/failures, but may hurt with memory for long lists..
  failures_df <- dplyr::filter(responses_df, !is_resp)
  successes_df <- dplyr::filter(responses_df, is_resp) |>
    dplyr::mutate(status_code = purrr::map(response, resp_status))

  return(responses_df)
}

