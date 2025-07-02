# oai_build_completions_request docs ----
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
# oai_build_completions_request docs ----
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
    if (inherits(schema, "EndpointR::json_schema")){
      schema <- json_dump(schema)
    }
    body$response_format <- schema
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

# oai_build_completions_request_list docs ----
#' Build OpenAI requests for batch processing
#'
#' @param inputs Character vector of text inputs
#' @param model OpenAI model to use
#' @param temperature Sampling temperature
#' @param max_tokens Maximum tokens per response
#' @param schema Optional JSON schema for structured output
#' @param system_prompt Optional system prompt
#' @param max_retries Integer; maximum retry attempts (default: 5)
#' @param timeout Numeric; request timeout in seconds (default: 30)
#' @param key_name Environment variable name for API key
#' @param endpoint_url OpenAI API endpoint URL
#'
#' @return List of httr2 request objects
#' @export
# oai_build_completions_request_list docs ----
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

# oai_complete_text docs ----
#' Generate a completion for a single text using OpenAI's Chat Completions API
#'
#' @description
#' High-level function to generate a completion for a single text string.
#' This function handles the entire process from request creation to
#' response processing, with optional structured output support.
#'
#' @param text Character string to send to the model
#' @param model OpenAI model to use (default: "gpt-4.1-nano")
#' @param system_prompt Optional system prompt to guide the model's behaviour
#' @param schema Optional JSON schema for structured output (json_schema object or list)
#' @param temperature Sampling temperature (0-2), lower = more deterministic (default: 0)
#' @param max_tokens Maximum tokens in response (default: 500)
#' @param key_name Environment variable name for API key (default: "OPENAI_API_KEY")
#' @param endpoint_url OpenAI API endpoint URL
#' @param max_retries Maximum retry attempts for failed requests (default: 5)
#' @param timeout Request timeout in seconds (default: 30)
#' @param tidy Whether to attempt to parse structured output (default: TRUE)
#'
#' @return Character string containing the model's response, or parsed JSON if schema provided
#' @export
#'
#' @examples
#' \dontrun{
#'   # Simple completion
#'   response <- oai_complete_text(
#'     text = "Explain quantum computing in simple terms",
#'     temperature = 0.7
#'   )
#'
#'   # With system prompt
#'   response <- oai_complete_text(
#'     text = "What are the main benefits?",
#'     system_prompt = "You are an expert in renewable energy",
#'     max_tokens = 200
#'   )
#'
#'   # Structured output with schema
#'   sentiment_schema <- create_json_schema(
#'     name = "sentiment_analysis",
#'     schema = schema_object(
#'       sentiment = schema_string("positive, negative, or neutral"),
#'       confidence = schema_number("confidence score between 0 and 1"),
#'       required = list("sentiment", "confidence")
#'     )
#'   )
#'
#'   result <- oai_complete_text(
#'     text = "I absolutely loved this product!",
#'     schema = sentiment_schema,
#'     temperature = 0
#'   )
#' }
# oai_complete_text docs ----
oai_complete_text <- function(text,
                              model = "gpt-4.1-nano",
                              system_prompt = NULL,
                              schema = NULL,
                              temperature = 0,
                              max_tokens = 500L,
                              key_name = "OPENAI_API_KEY",
                              endpoint_url = "https://api.openai.com/v1/chat/completions",
                              max_retries = 5L,
                              timeout = 30,
                              tidy = TRUE) {

  stopifnot(
    "text must be a single, non-empty character string" = is.character(text) &&
      length(text) == 1 &&
      nchar(text) > 0
  )

  req <- oai_build_completions_request(
    input = text,
    model = model,
    temperature = temperature,
    max_tokens = max_tokens,
    schema = schema,
    system_prompt = system_prompt,
    key_name = key_name,
    endpoint_url = endpoint_url,
    timeout = timeout,
    max_retries = max_retries
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

  # basic request sending with non-comprehensive error handling
  if (httr2::resp_status(response) != 200) {
    error_msg <- tryCatch(
      httr2::resp_body_json(response)$error$message,
      error = function(e) paste("HTTP", httr2::resp_status(response))
    )
    cli::cli_abort(c(
      "API request failed",
      "x" = error_msg
    ))
  }

  # status is 200 (success) if we got here, so we should be able to pluck response
  content <- .extract_oai_completion_content(response)

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

# oai_complete_df docs ----
#' Process a data frame through OpenAI's Chat Completions API
#'
#' This function takes a data frame with text inputs and processes each row through
#' OpenAI's Chat Completions API, returning results in a tidy format. It handles
#' concurrent requests, retries, and structured output validation automatically.
#'
#' @details This function streamlines processing of data through OpenAI's API.
#' It extracts the specified text column from your data frame, sends each text as a
#' separate API request, and returns the results joined back to your original data.
#'
#' The function preserves row identity through the `id_var` parameter, ensuring results
#' can be matched back to source data even if some requests fail. Failed requests are
#' marked with `.error = TRUE` and include error messages.
#'
#' When using structured outputs with a `schema`, the function automatically validates
#' and unnests (to the top level) the JSON responses into separate columns. This makes it ideal for
#' tasks like entity extraction, classification, or structured data generation.
#'
#' For best performance, adjust `concurrent_requests` based on your API rate limits.
#' Higher values speed up processing but may hit rate limits more frequently.
#'
#' @param df Data frame containing text to process
#' @param text_var Column name (unquoted) containing text inputs
#' @param id_var Column name (unquoted) for unique row identifiers
#' @param model OpenAI model to use (default: "gpt-4.1-nano")
#' @param system_prompt Optional system prompt applied to all requests
#' @param schema Optional JSON schema for structured output (json_schema object or list)
#' @param concurrent_requests Number of simultaneous API requests (default: 1)
#' @param max_retries Maximum retry attempts per request (default: 5)
#' @param timeout Request timeout in seconds (default: 30)
#' @param temperature Sampling temperature (0-2), lower = more deterministic (default: 0)
#' @param max_tokens Maximum tokens per response (default: 500)
#' @param progress Show progress bar (default: TRUE)
#' @param key_name Environment variable name for API key (default: "OPENAI_API_KEY")
#' @param endpoint_url OpenAI API endpoint URL
#'
#' @return A tibble
#'
#' @export
#' @examples
#' \dontrun{
#'
#' sample_texts <- rep("sample_text", 100)
#' # Parallel requests without schema:
#' large_df <- data.frame(
#'   doc_id = 1:100,
#'   text = sample_texts
#' )
#'
#' results <- oai_complete_df(
#'   large_df,
#'   text_var = text,
#'   id_var = doc_id,
#'   concurrent_requests = 5,  # process 5 at a time
#'   max_retries = 3
#' )
#'
#' # Structured outputs with a schema:
#' contact_schema <- create_json_schema(
#'  name = "contact_info",
#'  schema = schema_object(
#'   name = schema_string("person's full name"),
#'   email = schema_string("email address"),
#'   phone = schema_string("phone number"),
#'   required = list("name", "email", "phone"),
#'   additional_properties = FALSE
#' ))
#'
#' text <- "Am I speaking with Margaret Phillips?
#' Yes, ok, and your email is mphil@hotmail.co.uk.
#' Ok perfect, and your phone number? Was that 07564789789? Ok great.
#' Just a second please Margaret, you're verified"
#'
#' schema_df <- data.frame(id = 1, text = text)
#'
#' results <- oai_complete_df(
#'   schema_df,
#'   text_var = text,
#'   id_var = id,
#'   schema = contact_schema,
#'   temperature = 0  # recommended for structured outputs
#' )
#'
#' }
# oai_complete_df docs ----
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
  # Input validation ----
  text_sym <- rlang::ensym(text_var)
  id_sym <- rlang::ensym(id_var)

  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "df must not be empty" = nrow(df) > 0,
    "text_var must exist in df" = rlang::as_name(text_sym) %in% names(df),
    "id_var must exist in df" = rlang::as_name(id_sym) %in% names(df)
  )

  # build requests ----
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

  # track valid requests ----
  is_valid_request <- purrr::map_lgl(requests, ~inherits(.x, "httr2_request"))
  valid_indices <- ids[is_valid_request]
  valid_requests <- requests[is_valid_request]

  if (length(valid_requests) == 0) {
    cli::cli_abort("No valid requests could be created")
  }

  # perform requests ----
  responses <- perform_requests_with_strategy(
    valid_requests,
    concurrent_requests = concurrent_requests,
    progress = progress
  )

  # process responses ----
  results_df <- tibble::tibble(
    !!id_sym := valid_indices,
    response = responses
  ) |>
    dplyr::mutate(
      extracted = purrr::map(response, ~.extract_response_fields(.x, schema = schema))
    ) |>
    tidyr::unnest_wider(extracted) |>
    dplyr::select(-response)


  # validate against schema ----
  # in the old implementation, if the schema didn't validate we'd error - and lose any susccessful requests too. This is more complex, but it handles the edge cases better.

  if (!is.null(schema)) {
    # validate and track errors to make sure we don't try to unnest data that doesn't exist, and maintain homogeneity in output shape
    validation_errors <- purrr::map_chr(results_df$content, ~{
      if (!is.na(.x)) {
        tryCatch({
          validate_response(schema, .x)
          NA_character_
        }, error = function(e) as.character(e$message))
      } else {
        NA_character_
      }
    })

    results_df <- results_df |>
      dplyr::mutate(.error_msg = dplyr::coalesce(.error_msg, validation_errors))


    n_validation_errors <- sum(!is.na(validation_errors))

    if (n_validation_errors > 0) {
      cli::cli_warn(c(
        "{n_validation_errors} response{?s} failed schema validation",
        "i" = "Returning raw JSON in 'content' column for all rows"
      ))
    } else {
      # can only unnest safely if ALL validations passed, or we'd want to create a 'content' column for unsuccessful validations, and, e.g. 'sentiment' for successful validatons in the simple sentiment case...
      results_df <- results_df |>
        dplyr::mutate(
          content = purrr::map(content, ~validate_response(schema, .x))
        ) |>
        tidyr::unnest_wider(content)
    }
  }

  # add invalid requests back ----
  if (any(!is_valid_request)) {
    invalid_df <- tibble::tibble(
      !!id_sym := ids[!is_valid_request],
      status = NA_integer_,
      content = NA_character_,
      .error_msg = "Failed to create valid request"
    )

    results_df <- dplyr::bind_rows(results_df, invalid_df)
  }

  # final cleanup ----
  results_df <- results_df |>
    dplyr::mutate(
      .error = !is.na(.error_msg),
      !!text_sym := inputs[match(!!id_sym, ids)]  # add original text back
    ) |>
    dplyr::arrange(!!id_sym) |>
    dplyr::select(!!id_sym, !!text_sym, dplyr::everything())


  n_success <- sum(!results_df$.error)
  n_failed <- sum(results_df$.error)

  cli::cli_alert_success("Completed: {n_success} successful, {n_failed} failed")


  return(results_df)
}



# helper function for oai_complete_df()
# extract all needed fields from a response object
# check if response is valid httr2_response
# if not valid, return NA values with error message
# if valid, extract status code
# if status is 200, try to extract content
# if status is not 200, extract error message from response body
# if schema provided and content exists, try to parse as json
# return a list with all extracted fields
#' @keywords internal
.extract_response_fields <- function(response, schema = NULL) {
  if (!inherits(response, "httr2_response")) {
    return(list(
      status = NA_integer_,
      content = NA_character_,
      .error_msg = "Invalid response object"
    ))
  }

  status <- httr2::resp_status(response)

  if (status == 200) {
    content <- tryCatch(
      .extract_oai_completion_content(response),
      error = function(e) NA_character_
    )

    return(list(
      status = status,
      content = content,
      .error_msg = NA_character_
    ))
  } else {
    .error_msg <- tryCatch(
      httr2::resp_body_json(response)$error$message,
      error = function(e) paste("HTTP", status)
    )

    return(list(
      status = status,
      content = NA_character_,
      .error_msg = .error_msg
    ))
  }
}


#' @keywords internal
.extract_oai_completion_content <- function(resp) {
  resp |>
    httr2::resp_body_json() |>
    purrr::pluck("choices", 1, "message", "content")
}
