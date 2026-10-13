# constants ----
.ANT_API_VERSION <- "2023-06-01"
.ANT_MESSAGES_ENDPOINT <- "https://api.anthropic.com/v1/messages"
.ANT_DEFAULT_MODEL <- "claude-haiku-5-5"
# models that reject sampling parameters (temperature/top_p/top_k) with a 400:
# Claude Opus 4.7+, Haiku 5, Sonnet 5, Fable 5, Mythos 5. Add new families as Anthropic releases them.
.ANT_NO_SAMPLING_REGEX <- "fable|mythos|opus-4-[789]|opus-5|sonnet-5|haiku-5"
.ANT_EFFORT_LEVELS <- c("low", "medium", "high", "xhigh", "max")
.ANT_THINKING_TYPES <- c("adaptive", "disabled", "between_tools")

# model rules for effort and thinking, checked against
# https://platform.claude.com/docs/en/build-with-claude/thinking-troubleshooting#supported-models
# and https://platform.claude.com/docs/en/build-with-claude/effort on 8 October 2026.
# A model that matches none of these goes to the API unchecked. Update them as Anthropic release models.
# no effort: Haiku 4.5, Sonnet 4.5 and older Claude 4 models (Opus 4.1, Opus 4, Sonnet 4)
.ANT_NO_EFFORT_REGEX <- "haiku-4|sonnet-4-[0-5]|opus-4-[0-4]"
# effort, but no "xhigh": Opus 4.5, Opus 4.6, Sonnet 4.6, Mythos Preview
.ANT_NO_XHIGH_REGEX <- "opus-4-[56]|sonnet-4-6|mythos-preview"
# effort, but no "max": Opus 4.5
.ANT_NO_MAX_REGEX <- "opus-4-5"
# fixed budget only, no adaptive thinking: the 4.5 models and older Claude 4 models
.ANT_NO_ADAPTIVE_REGEX <- "(haiku|sonnet|opus)-4-[0-5]"
# fixed budget returns a 400: Claude 4.7 onwards (Mythos Preview still accepts one)
.ANT_NO_BUDGET_REGEX <- "fable|mythos-5|opus-4-[789]|opus-5|sonnet-5|haiku-5"
# thinking can't be disabled: Fable, Mythos, Opus 5.5, and Sonnet 5.5 (which takes "between_tools" instead)
.ANT_NO_DISABLED_REGEX <- "fable|mythos|opus-5-5|sonnet-5-5"
# thinking off ("disabled" or "between_tools") only at effort "high" or lower: Opus 5, Haiku 5.5, Sonnet 5.5
.ANT_THINKING_OFF_MAX_HIGH_REGEX <- "opus-5($|-[0-9]{8})|haiku-5|sonnet-5-5"
# the only models that accept "between_tools": Sonnet 5.5
.ANT_BETWEEN_TOOLS_REGEX <- "sonnet-5-5"
# Claude model ids, used so that "between_tools" is only stopped on Claude models, not on compatible providers
.ANT_CLAUDE_REGEX <- "claude|fable|mythos|opus|sonnet|haiku"

# ant_build_messages_request ----
#' Build an Anthropic Messages API request
#'
#' @description
#' Constructs an httr2 request object for Anthropic's Messages API.
#' Handles message formatting, system prompts, and optional JSON schema
#' for structured outputs.
#'
#' @details
#' This function creates the HTTP request but does not execute it.
#'
#' The `schema` parameter accepts either:
#' - A `json_schema` S7 object created with `create_json_schema()`
#' - A raw list in Anthropic's `output_config` structure
#'
#' Anthropic uses `output_config` with a nested `format` field for structured
#' outputs. This is now GA and requires no beta header.
#'
#' @section Effort and thinking:
#' Most users only need `effort`. It controls how much the model thinks and
#' how many tokens it uses overall. Lower effort is cheaper and faster. The
#' default is "high" on most models, and "medium" on Claude Opus 5.5 and
#' Claude Haiku 5.5. Haiku 4.5 and Sonnet 4.5 don't accept `effort`.
#'
#' Use `thinking` for two things: to turn thinking on for older models, and to
#' turn it off on the models that allow it.
#' - Haiku 4.5, Sonnet 4.5 and Opus 4.5 only think with a fixed budget, e.g.
#'   `thinking = 2048`. The budget must be at least 1024 and less than
#'   `max_tokens`.
#' - Models from 4.6 onwards use `thinking = "adaptive"`, and models from 4.7
#'   onwards reject a fixed budget.
#' - `thinking = "disabled"` turns thinking off on Opus 4.7, Opus 4.8, Sonnet 5,
#'   Opus 5 and Haiku 5.5. Opus 5 and Haiku 5.5 only accept it at effort "high"
#'   or lower.
#' - Sonnet 5.5 uses `thinking = "between_tools"` instead, at effort "high" or
#'   lower.
#' - Opus 5.5, Fable and Mythos can't turn thinking off. Lower `effort` instead.
#'
#' EndpointR checks these rules before sending, and stops with a message that
#' suggests a setting that works. Models it doesn't recognise are sent
#' unchecked.
#'
#' Thinking tokens count towards `max_tokens`. If a model spends all of
#' `max_tokens` thinking, the response has no text, `content` is `NA`, and
#' `stop_reason` is "max_tokens". Raise `max_tokens`, or lower `effort` or the
#' thinking budget, if that happens. A thinking budget is a target rather than
#' a hard limit. In our tests, Haiku 4.5 with `thinking = 2048` sometimes
#' thought for over 4000 tokens, so leave plenty of room above the budget.
#'
#' Anthropic reject a changed `temperature` while thinking is on, so when
#' `thinking` is "adaptive" or a budget, `temperature` is dropped with a
#' warning.
#'
#' Before you submit a large batch, send one request with
#' [ant_complete_text()] using the same settings. The API only reports a bad
#' setting in a batch after the whole batch has run.
#'
#' @param input Text input to send to the model
#' @param endpointr_id An id that will persist through to response
#' @param model Anthropic model to use (default: "claude-haiku-5-5")
#' @param temperature Sampling temperature (0-1), included in the request only when non-NULL. Dropped with a warning on models that reject sampling parameters (Claude Opus 4.7+, Haiku 5.5, Sonnet 5, Fable 5), and when `thinking` is "adaptive" or a budget.
#' @param max_tokens Maximum tokens in the response, including thinking tokens
#' @param schema Optional JSON schema for structured output (json_schema object or list)
#' @param system_prompt Optional system prompt. When provided, prompt caching
#'   is automatically enabled via `cache_control`, reducing costs for repeated
#'   requests sharing the same system prompt.
#' @param key_name Environment variable name for API key
#' @param endpoint_url Anthropic API endpoint URL
#' @param timeout Request timeout in seconds
#' @param max_retries Maximum number of retry attempts for failed requests
#' @param effort Optional effort level, one of "low", "medium", "high", "xhigh", "max". Sent as `output_config$effort`. Not accepted by Haiku 4.5 or Sonnet 4.5. See the "Effort and thinking" section.
#' @param thinking Optional thinking setting. `NULL` (the default) leaves it out of the request, so the model uses its own default. One of "adaptive", "disabled" or "between_tools", or a whole number for a fixed thinking budget in tokens. See the "Effort and thinking" section.
#'
#' @return An httr2 request object
#' @export
#'
#' @seealso \url{https://platform.claude.com/docs/en/build-with-claude/structured-outputs},
#'   \url{https://platform.claude.com/docs/en/build-with-claude/effort}
#' @examples
#' \dontrun{
#'   # simple request
#'   req <- ant_build_messages_request(
#'     input = "What is the capital of France?",
#'     max_tokens = 100
#'   )
#'
#'   # less thinking on Haiku 5.5
#'   req <- ant_build_messages_request(
#'     input = "What is the capital of France?",
#'     effort = "low"
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
  max_tokens = 4096L,
  schema = NULL,
  system_prompt = NULL,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_MESSAGES_ENDPOINT,
  timeout = 120L,
  max_retries = 5L,
  effort = NULL,
  thinking = NULL
  ) {
  # can't use `base_request()` from core.R because Anthropic use different auth (x-api-key) so we add as a header

  stopifnot(
    "input must be a non-empty character string" = is.character(input) && length(input) == 1 && nchar(input) > 0
  )

  api_key <- get_api_key(key_name)

  body <- .ant_build_body(
    input = input,
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    system_prompt = system_prompt,
    schema = schema,
    effort = effort,
    thinking = thinking,
    call = rlang::current_env()
  )

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

  if (!is.null(endpointr_id)) {
    request <- httr2::req_headers(request, endpointr_id = endpointr_id)
  }

  return(request)
}
# ant_build_messages_request ----

# .ant_build_body ----
#' Build the body of an Anthropic Messages request
#'
#' Shared by the Messages route and the Batches route (where it becomes each
#' request's `params`), so the two routes send the same fields. Checks the
#' arguments, drops `temperature` where the API would reject it, and stops
#' when `effort` and `thinking` can't work on `model`.
#'
#' @inheritParams ant_build_messages_request
#' @param call The user-facing function to name in error messages
#' @return A list, ready to be sent as JSON
#' @keywords internal
.ant_build_body <- function(input,
                            model,
                            max_tokens,
                            temperature,
                            system_prompt,
                            schema,
                            effort,
                            thinking,
                            call = rlang::caller_env()) {

  stopifnot(
    "model must be a character string" = is.character(model) && length(model) == 1,
    "temperature must be numeric between 0 and 1" = is.null(temperature) || (is.numeric(temperature) && temperature >= 0 && temperature <= 1), # diff to OAI API
    "max_tokens must be a positive integer" = is.numeric(max_tokens) && max_tokens > 0)

  if (!is.null(effort)) {
    effort <- rlang::arg_match(effort, .ANT_EFFORT_LEVELS)
  }
  thinking_config <- .ant_thinking_config(thinking, call = call)

  .ant_check_effort_thinking(model, effort, thinking_config, max_tokens, call = call)

  # Claude Opus 4.7+, Haiku 5.5, Sonnet 5 and Fable 5 reject sampling parameters with a 400
  if (!is.null(temperature) && grepl(.ANT_NO_SAMPLING_REGEX, model)) {
    cli::cli_warn(
      "{.arg temperature} is not supported by {.val {model}} and has been dropped from the request.",
      .frequency = "once",
      .frequency_id = "ant_sampling_drop"
    )
    temperature <- NULL
  }

  # Anthropic reject a changed temperature while thinking is on
  if (!is.null(temperature) && !is.null(thinking_config) && thinking_config$type %in% c("adaptive", "enabled")) {
    cli::cli_warn(
      "{.arg temperature} can't be changed while thinking is on, so it has been dropped from the request.",
      .frequency = "once",
      .frequency_id = "ant_thinking_temperature_drop"
    )
    temperature <- NULL
  }

  body <- list(
    model = model,
    messages = list(
      list(role = "user", content = input)
    ),
    max_tokens = as.integer(max_tokens)
  )

  if (!is.null(temperature)) {
    body$temperature <- temperature
  }

  # Anthropic API takes system_prompt as its own parameter, different to OAI where we concatenate
  if (!is.null(system_prompt)) {
    if (!rlang::is_scalar_character(system_prompt)) {
      cli::cli_abort("{.arg system_prompt} must be a {.cls character} of length 1, e.g. 'This is a valid system prompt'", call = call)
    }

    body$system <- list(
      list(
        type = "text",
        text = system_prompt,
        cache_control = list(type = "ephemeral")
      )
    )
  }

  if (!is.null(schema)) {
    if (inherits(schema, "EndpointR::json_schema")) {
      body$output_config <- .ant_format_schema(schema)
    } else if (is.list(schema)) {
      body$output_config <- schema
    } else {
      cli::cli_abort("{.arg schema} must be an EndpointR json_schema object or a list", call = call)
    }
  }

  # effort lives in output_config alongside any structured output format
  if (!is.null(effort)) {
    body$output_config$effort <- effort
  }

  if (!is.null(thinking_config)) {
    body$thinking <- thinking_config
  }

  body
}

#' Turn the `thinking` argument into the API's thinking object
#' @return `NULL`, or a list with `type` (and `budget_tokens` for a fixed budget)
#' @keywords internal
.ant_thinking_config <- function(thinking, call = rlang::caller_env()) {
  if (is.null(thinking)) {
    return(NULL)
  }

  if (is.numeric(thinking)) {
    if (length(thinking) != 1 || is.na(thinking) || thinking != round(thinking)) {
      cli::cli_abort("A {.arg thinking} budget must be a single whole number of tokens, e.g. {.code thinking = 2048}.", call = call)
    }
    return(list(type = "enabled", budget_tokens = as.integer(thinking)))
  }

  if (!rlang::is_scalar_character(thinking)) {
    cli::cli_abort(c(
      "{.arg thinking} must be {.code NULL}, one of {.or {.val {(.ANT_THINKING_TYPES)}}}, or a whole number of tokens.",
      "x" = "You supplied {.obj_type_friendly {thinking}}."
    ), call = call)
  }

  thinking <- rlang::arg_match(thinking, .ANT_THINKING_TYPES, error_call = call)
  list(type = thinking)
}

#' Stop when `effort` and `thinking` can't work on `model`
#'
#' The API checks requests in a batch only after the whole batch has run, so
#' we check the rules we know about before sending. Models that match none of
#' the rule regexes are not checked.
#' @keywords internal
.ant_check_effort_thinking <- function(model, effort, thinking_config, max_tokens, call = rlang::caller_env()) {
  has_model <- function(regex) grepl(regex, model, perl = TRUE)

  if (!is.null(effort)) {
    if (has_model(.ANT_NO_EFFORT_REGEX)) {
      cli::cli_abort(c(
        "{.val {model}} doesn't accept {.arg effort}.",
        "i" = "Leave {.arg effort} as {.code NULL}, or use a model that accepts it, such as {.val claude-haiku-5-5}."
      ), call = call)
    }
    if (effort == "xhigh" && has_model(.ANT_NO_XHIGH_REGEX)) {
      cli::cli_abort(c(
        "{.val {model}} doesn't accept {.code effort = \"xhigh\"}.",
        "i" = "Use {.code effort = \"high\"} instead."
      ), call = call)
    }
    if (effort == "max" && has_model(.ANT_NO_MAX_REGEX)) {
      cli::cli_abort(c(
        "{.val {model}} doesn't accept {.code effort = \"max\"}.",
        "i" = "Use {.code effort = \"high\"} instead."
      ), call = call)
    }
  }

  if (is.null(thinking_config)) {
    return(invisible(NULL))
  }

  type <- thinking_config$type

  if (type == "enabled") {
    budget <- thinking_config$budget_tokens
    if (has_model(.ANT_NO_BUDGET_REGEX)) {
      cli::cli_abort(c(
        "{.val {model}} doesn't accept a fixed thinking budget.",
        "i" = "Use {.code thinking = \"adaptive\"} and set {.arg effort} to control how much the model thinks."
      ), call = call)
    }
    if (budget < 1024) {
      cli::cli_abort(c(
        "A {.arg thinking} budget must be at least 1024 tokens.",
        "x" = "You supplied {budget}.",
        "i" = "Use {.code thinking = 1024} or more."
      ), call = call)
    }
    if (budget >= max_tokens) {
      cli::cli_abort(c(
        "A {.arg thinking} budget must be less than {.arg max_tokens}.",
        "x" = "{.arg thinking} is {budget} and {.arg max_tokens} is {max_tokens}.",
        "i" = "Raise {.arg max_tokens} above {budget}, e.g. {.code max_tokens = {budget + 1024}}, to leave room for the answer."
      ), call = call)
    }
  }

  if (type == "adaptive" && has_model(.ANT_NO_ADAPTIVE_REGEX)) {
    cli::cli_abort(c(
      "{.val {model}} doesn't accept {.code thinking = \"adaptive\"}.",
      "i" = "Use a fixed budget instead, e.g. {.code thinking = 2048}."
    ), call = call)
  }

  if (type == "disabled" && has_model(.ANT_NO_DISABLED_REGEX)) {
    hint <- if (has_model(.ANT_BETWEEN_TOOLS_REGEX)) {
      "Use {.code thinking = \"between_tools\"} to turn thinking off on this model."
    } else {
      "Thinking can't be turned off on this model. Leave {.arg thinking} as {.code NULL} and use {.code effort = \"low\"} to reduce it."
    }
    cli::cli_abort(c(
      "{.val {model}} doesn't accept {.code thinking = \"disabled\"}.",
      "i" = hint
    ), call = call)
  }

  if (type == "between_tools" && has_model(.ANT_CLAUDE_REGEX) && !has_model(.ANT_BETWEEN_TOOLS_REGEX)) {
    hint <- if (has_model(.ANT_NO_DISABLED_REGEX)) {
      "Thinking can't be turned off on this model. Leave {.arg thinking} as {.code NULL} and use {.code effort = \"low\"} to reduce it."
    } else {
      "Use {.code thinking = \"disabled\"} to turn thinking off on this model."
    }
    cli::cli_abort(c(
      "{.val {model}} doesn't accept {.code thinking = \"between_tools\"}. Only Claude Sonnet 5.5 does.",
      "i" = hint
    ), call = call)
  }

  if (type %in% c("disabled", "between_tools") &&
      !is.null(effort) && effort %in% c("xhigh", "max") &&
      has_model(.ANT_THINKING_OFF_MAX_HIGH_REGEX)) {
    cli::cli_abort(c(
      "{.val {model}} only accepts {.code thinking = \"{type}\"} at effort {.val high} or lower.",
      "x" = "You supplied {.code effort = \"{effort}\"}.",
      "i" = "Use {.code effort = \"high\"}, or leave {.arg thinking} as {.code NULL} to keep thinking on."
    ), call = call)
  }

  invisible(NULL)
}


# ant_complete_text ----
#' Generate a completion for a single text using Anthropic's Messages API
#'
#' @description
#' High-level function to generate a completion for a single text string.
#' Handles request creation, execution, and response processing with
#' optional structured output support.
#'
#' @param text Character string to send to the model
#' @param system_prompt Optional system prompt
#' @param schema Optional JSON schema for structured output
#' @param key_name Environment variable name for API key
#' @param endpoint_url Anthropic API endpoint URL
#' @param max_retries Maximum retry attempts
#' @param timeout Request timeout in seconds
#' @param tidy Whether to parse structured output (default: TRUE)
#' @inheritParams ant_build_messages_request
#' @inheritSection ant_build_messages_request Effort and thinking
#'
#' @return Character string with the model's response, or parsed JSON if schema provided.
#'   `NA` when the model ran out of tokens before writing an answer, with a warning.
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
#'   # test your settings on one text before running a large batch
#'   ant_complete_text(
#'     text = "I love this product!",
#'     model = "claude-sonnet-5-5",
#'     effort = "low",
#'     thinking = "between_tools"
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
                              max_tokens = 4096L,
                              key_name = "ANTHROPIC_API_KEY",
                              endpoint_url = .ANT_MESSAGES_ENDPOINT,
                              max_retries = 5L,
                              timeout = 120L,
                              tidy = TRUE,
                              effort = NULL,
                              thinking = NULL) {

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
    key_name = key_name,
    endpoint_url = endpoint_url,
    max_retries = max_retries,
    timeout = timeout,
    system_prompt = system_prompt,
    effort = effort,
    thinking = thinking
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
      "x" = "{error_msg}"
    ))
  }

  fields <- .ant_message_fields(httr2::resp_body_json(response))
  content <- fields$content
  .ant_warn_out_of_tokens(content, fields$stop_reason)

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
#' When using a custom `output_dir`, existing chunk files are protected
#' by default. Set `overwrite = TRUE` to replace them.
#'
#' The results have the same `stop_reason`, `input_tokens`, `output_tokens`
#' and `thinking_tokens` columns as [ant_batch_results()]. `thinking_tokens`
#' is `NA` when the API doesn't report it.
#'
#' @param texts Character vector of texts to process
#' @param ids Vector of unique identifiers (same length as texts)
#' @param chunk_size Number of texts per chunk before writing to disk
#' @param system_prompt Optional system prompt (applied to all requests).
#'   Prompt caching is enabled automatically, reducing costs when the same
#'   system prompt is shared across many requests.
#' @param output_dir Directory for parquet chunks ("auto" generates timestamped dir)
#' @param overwrite If `FALSE` (default), errors when `output_dir` already contains chunk (`.parquet`) or `metadata.json` files. Set to `TRUE` to delete them and write fresh outputs; other files are left untouched.
#' @param schema Optional JSON schema for structured output
#' @param concurrent_requests Number of concurrent requests
#' @param max_retries Maximum retry attempts per request
#' @param timeout Request timeout in seconds
#' @param key_name Environment variable name for API key
#' @param endpoint_url Anthropic API endpoint URL
#' @param id_col_name Name for ID column in output
#' @inheritParams ant_build_messages_request
#' @inheritSection ant_build_messages_request Effort and thinking
#'
#' @return A tibble with columns: the id column, `content`, `.error`,
#'   `.error_msg`, `stop_reason`, `input_tokens`, `output_tokens`,
#'   `thinking_tokens`, `.status` and `.chunk`
#' @export
ant_complete_chunks <- function(texts,
                                ids,
                                chunk_size = 5000L,
                                model = .ANT_DEFAULT_MODEL,
                                system_prompt = NULL,
                                output_dir = "auto",
                                overwrite = FALSE,
                                schema = NULL,
                                concurrent_requests = 5L,
                                temperature = 0,
                                max_tokens = 4096L,
                                max_retries = 5L,
                                timeout = 120L,
                                key_name = "ANTHROPIC_API_KEY",
                                endpoint_url = .ANT_MESSAGES_ENDPOINT,
                                id_col_name = "id",
                                effort = NULL,
                                thinking = NULL) {

  stopifnot(
    "texts must be a vector" = is.vector(texts),
    "ids must be a vector" = is.vector(ids),
    "texts and ids must be the same length" = length(texts) == length(ids),
    "chunk_size must be a positive integer" = is.numeric(chunk_size) && chunk_size > 0
  )

  # check the settings once, so a bad setting stops here rather than inside the loop
  .ant_build_body(
    input = "settings check",
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    system_prompt = system_prompt,
    schema = schema,
    effort = effort,
    thinking = thinking
  )

  output_dir <- .handle_output_directory(output_dir, base_dir_name = "ant_messages_chunks")
  .check_existing_output(output_dir, overwrite = overwrite)

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
    system_prompt = system_prompt,
    chunk_size = chunk_size,
    n_chunks = n_chunks,
    n_texts = length(texts),
    concurrent_requests = concurrent_requests,
    timeout = timeout,
    max_retries = max_retries,
    max_tokens = max_tokens,
    temperature = temperature,
    effort = effort,
    thinking = thinking,
    key_name = key_name,
    timestamp = Sys.time()
  )

  if (!is.null(formatted_schema)) {
    metadata$schema <- formatted_schema
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
        timeout = timeout,
        effort = effort,
        thinking = thinking
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
      successes_fields <- purrr::map(successes, \(x) .ant_message_fields(httr2::resp_body_json(x))) |>
        purrr::list_transpose(simplify = TRUE)

      chunk_results$successes <- tibble::tibble(
        !!id_col_name := successes_ids,
        content = successes_fields$content,
        .error = FALSE,
        .error_msg = NA_character_,
        stop_reason = successes_fields$stop_reason,
        input_tokens = successes_fields$input_tokens,
        output_tokens = successes_fields$output_tokens,
        thinking_tokens = successes_fields$thinking_tokens,
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
        stop_reason = NA_character_,
        input_tokens = NA_integer_,
        output_tokens = NA_integer_,
        thinking_tokens = NA_integer_,
        .status = failures_status,
        .chunk = chunk_num
      )
    }

    chunk_df <- dplyr::bind_rows(chunk_results)

    if (nrow(chunk_df) > 0) {
      chunk_file <- glue::glue("{output_dir}/chunk_{sprintf('%03d', as.integer(chunk_num))}.parquet")
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

  .ant_warn_out_of_tokens(final_results$content, final_results$stop_reason)

  return(final_results)
}
# ant_complete_chunks ----

# ant_complete_df ----
#' Process a data frame through Anthropic's Messages API
#'
#' @description
#' Takes a data frame with text inputs and processes each row through
#' Anthropic's Messages API using chunked processing. Results are written
#' progressively to parquet files and returned as a tibble. Supports
#' structured outputs via the schema = argument.
#'
#' @details
#' writes results to disk to minimise memory usage and possibility of data loss.
#'
#' Results are written as parquet files in the specified output directory,
#' along with a metadata.json file containing processing parameters.
#'
#' When using a custom `output_dir`, existing chunk files are protected
#' by default. Set `overwrite = TRUE` to replace them.
#'
#' The results have the same `stop_reason`, `input_tokens`, `output_tokens`
#' and `thinking_tokens` columns as [ant_batch_results()]. `thinking_tokens`
#' is `NA` when the API doesn't report it.
#'
#' @param df Data frame containing text to process
#' @param text_var Column name (unquoted) containing text inputs
#' @param id_var Column name (unquoted) for unique row identifiers
#' @param output_dir Directory for parquet chunks
#' @param overwrite If `FALSE` (default), errors when `output_dir` already contains chunk (`.parquet`) or `metadata.json` files. Set to `TRUE` to delete them and write fresh outputs; other files are left untouched.
#' @param system_prompt Optional system prompt
#' @param schema Optional JSON schema for structured output
#' @param chunk_size Number of texts per chunk
#' @param concurrent_requests Number of concurrent requests
#' @param max_retries Maximum retry attempts
#' @param timeout Request timeout in seconds
#' @param key_name Environment variable name for API key
#' @param endpoint_url Anthropic API endpoint URL
#' @inheritParams ant_build_messages_request
#' @inheritSection ant_build_messages_request Effort and thinking
#'
#' @return A tibble with columns: `id_var`, `content`, `.error`,
#'   `.error_msg`, `stop_reason`, `input_tokens`, `output_tokens`,
#'   `thinking_tokens`, `.status` and `.chunk`
#' @export
#'
ant_complete_df <- function(df,
                            text_var,
                            id_var,
                            model = .ANT_DEFAULT_MODEL,
                            output_dir = "auto",
                            overwrite = FALSE,
                            system_prompt = NULL,
                            schema = NULL,
                            chunk_size = 5000L,
                            concurrent_requests = 5L,
                            max_retries = 5L,
                            timeout = 120L,
                            temperature = 0,
                            max_tokens = 4096L,
                            key_name = "ANTHROPIC_API_KEY",
                            endpoint_url = .ANT_MESSAGES_ENDPOINT,
                            effort = NULL,
                            thinking = NULL) {

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
    overwrite = overwrite,
    id_col_name = id_col_name,
    effort = effort,
    thinking = thinking
  )

  results <- dplyr::rename(results, !!id_sym := !!rlang::sym(id_col_name))

  return(results)
}

# ant_complete_df ----

#' Extract text content from Anthropic Messages API response
#' @keywords internal
.extract_ant_message_content <- function(resp) {
  .extract_content_from_batch_message(httr2::resp_body_json(resp))
}

#' Pull the result fields out of a parsed Anthropic message
#'
#' Used for Messages responses and for the `message` in a succeeded batch
#' result, which have the same shape, so both routes return the same columns.
#' @param msg A parsed message, e.g. from `httr2::resp_body_json()`
#' @return A list of `content`, `stop_reason`, `input_tokens`,
#'   `output_tokens` and `thinking_tokens`
#' @keywords internal
.ant_message_fields <- function(msg) {
  list(
    content = .extract_content_from_batch_message(msg),
    stop_reason = msg$stop_reason %||% NA_character_,
    input_tokens = as.integer(msg$usage$input_tokens %||% NA_integer_),
    output_tokens = as.integer(msg$usage$output_tokens %||% NA_integer_),
    thinking_tokens = as.integer(msg$usage$output_tokens_details$thinking_tokens %||% NA_integer_)
  )
}

#' Warn when responses ran out of tokens before writing any text
#'
#' Thinking tokens count towards `max_tokens`, so a model can spend all of
#' them thinking and return no text.
#' @keywords internal
.ant_warn_out_of_tokens <- function(content, stop_reason) {
  n_empty <- sum(is.na(content) & stop_reason %in% "max_tokens")

  if (n_empty > 0) {
    cli::cli_warn(c(
      "{n_empty} response{?s} ran out of tokens before writing an answer, so {?its/their} {.field content} is {.val {NA}}.",
      "i" = "Thinking tokens count towards {.arg max_tokens}. Raise {.arg max_tokens}, or lower {.arg effort} or the {.arg thinking} budget."
    ))
  }

  invisible(n_empty)
}


#' Convert json_schema S7 object to Anthropic output_config structure
#' @keywords internal
.ant_format_schema <- function(schema) {
  if (!inherits(schema, "EndpointR::json_schema")) {
    cli::cli_abort("schema must be a json_schema object")
  }

  # Anthropic uses output_config with a nested format field (GA, no beta header needed)
  list(
    format = list(
      type = "json_schema",
      schema = schema@schema
    )
  )
}

