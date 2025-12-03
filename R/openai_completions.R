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
#' @param endpointr_id An id that will persist through to response
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
    endpointr_id = NULL,
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

      # cli::cli_alert_info("calling json_dump")
      schema <- json_dump(schema)
    }
    body$response_format <- schema # we're trusting the user supplies this correctly, at the moment.
  }

  request <- base_request(endpoint_url = endpoint_url,
                          api_key = api_key) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = max_retries,
                     backoff = ~ 2 ^ .x,
                     retry_on_failure = TRUE) |>
    httr2::req_body_json(body)

  if(!is.null(endpointr_id)) {
    request <- httr2::req_headers(request, endpointr_id = endpointr_id)
  }

  return(request)
}

# oai_build_completions_request_list docs ----
#' Build OpenAI requests for batch processing
#'
#' @param inputs Character vector of text inputs
#' @param endpointr_ids A vector of IDs which will persist through to responses
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
    endpointr_ids = NULL,
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
    "inputs must not be empty" = length(inputs) > 0,
    "if `endpointr_ids` are supplied they must be the same length as `inputs`" = is.null(endpointr_ids) || length(inputs) == length(endpointr_ids)
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

  if(!is.null(endpointr_ids)) {
    requests <- purrr::map2(.x = requests,
                            .y = endpointr_ids,
                            .f = ~ httr2::req_headers(.x, endpointr_id = .y)
                            )
  }

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

# oai_complete_chunks docs ----
#' Process text chunks through OpenAI's Chat Completions API with batch file output
#'
#' This function processes large volumes of text through OpenAI's Chat Completions API
#' in configurable chunks, writing results progressively to a CSV file. It handles
#' concurrent requests, automatic retries, and structured outputs while
#' managing memory efficiently for large-scale processing.
#'
#' @details This function is designed for processing large text datasets that may not
#' fit comfortably in memory. It divides the input into chunks, processes each chunk
#' with concurrent API requests, and writes results immediately to disk to minimise
#' memory usage.
#'
#' The function preserves data integrity by matching results to source texts through
#' the `ids` parameter. Each chunk is processed independently with results appended
#' to the output file, allowing for resumable processing if interrupted.
#'
#' When using structured outputs with a `schema`, responses are validated against
#' the JSON schema but stored as raw JSON strings in the output file. This allows
#' for flexible post-processing without memory constraints during the API calls.
#'
#' The chunking strategy balances API efficiency with memory management. Larger
#' `chunk_size` values reduce overhead but increase memory usage. Adjust based on
#' your system resources and text sizes.
#'
#' @param texts Character vector of texts to process
#' @param ids Vector of unique identifiers corresponding to each text (same length as texts)
#' @param chunk_size Number of texts to process in each batch (default: 5000)
#' @param model OpenAI model to use (default: "gpt-4.1-nano")
#' @param system_prompt Optional system prompt applied to all requests
#' @param output_file Path to .CSV file for results. "auto" generates the filename, location and is persistent across sessions. If NULL, generates timestamped filename.
#' @param schema Optional JSON schema for structured output (json_schema object or list)
#' @param concurrent_requests Integer; number of concurrent requests (default: 5)
#' @param temperature Sampling temperature (0-2), lower = more deterministic (default: 0)
#' @param max_tokens Maximum tokens per response (default: 500)
#' @param max_retries Maximum retry attempts per failed request (default: 5)
#' @param timeout Request timeout in seconds (default: 30)
#' @param key_name Name of environment variable containing the API key (default: OPENAI_API_KEY)
#' @param endpoint_url OpenAI API endpoint URL
#'
#' @return A tibble containing all results with columns:
#'   - `id`: Original identifier from input
#'   - `content`: API response content (text or JSON string if schema used)
#'   - `.error`: Logical indicating if request failed
#'   - `.error_msg`: Error message if failed, NA otherwise
#'   - `.batch`: Batch number for tracking
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage with automatic file naming
#' texts <- c("Great product!", "Awful service.", "Decent value.")
#' ids <- c("review_1", "review_2", "review_3")
#'
#' results <- oai_complete_chunks(
#'   texts = texts,
#'   ids = ids,
#'   system_prompt = "Classify sentiment as positive, negative, or neutral."
#' )
#'
#' # Large-scale processing with custom output file
#' results <- oai_complete_chunks(
#'   texts = large_text_vector,
#'   ids = seq_along(large_text_vector),
#'   chunk_size = 2000,
#'   concurrent_requests = 10,
#'   output_file = "data/sentiment_results.csv"
#' )
#'
#' # Structured extraction with schema
#' sentiment_schema <- create_json_schema(
#'   name = "sentiment_analysis",
#'   schema = schema_object(
#'     sentiment = schema_string("positive, negative, or neutral"),
#'     confidence = schema_number("confidence score between 0 and 1"),
#'     required = list("sentiment", "confidence")
#'   )
#' )
#'
#' results <- oai_complete_chunks(
#'   texts = texts,
#'   ids = ids,
#'   schema = sentiment_schema,
#'   temperature = 0
#' )
#'
#' # Post-process structured results
#' results |>
#'   dplyr::filter(!.error) |>
#'   dplyr::mutate(parsed = purrr::map(content, safely_from_json)) |>
#'   tidyr::unnest_wider(parsed)
#' }
# oai_complete_chunks docs ----
oai_complete_chunks <- function(texts,
                               ids,
                               chunk_size = 5000L,
                               model = "gpt-4.1-nano",
                               system_prompt = NULL,
                               output_file = "auto",
                               schema = NULL,
                               concurrent_requests = 5L,
                               temperature = 0L,
                               max_tokens = 500L,
                               max_retries = 5L,
                               timeout = 30L,
                               key_name = "OPENAI_API_KEY",
                               endpoint_url = "https://api.openai.com/v1/chat/completions"
) {
  # input validation ----
  stopifnot(
    "texts must be a vector" = is.vector(texts),
    "ids must be a vector" = is.vector(ids),
    "texts and ids must be the same length" = length(texts) == length(ids),
    "chunk_size must be a positive integer greater than 1" = is.numeric(chunk_size) && chunk_size > 0
  )

  output_file <- .handle_output_filename(output_file)

  # make sure we json_dump the schema here if necessary, so that we don't json_dump for every individual document
  if(!is.null(schema) && inherits(schema, "EndpointR::json_schema")) {
    dumped_schema <- json_dump(schema)
  } else {
    dumped_schema <- schema
  }

  batch_data <- batch_vector(seq_along(texts), chunk_size)
  n_batches <- length(batch_data$batch_indices)

  cli::cli_alert_info("Processing {length(texts)} text{?s} in {n_batches} chunk{?s} of up to {chunk_size} each")

  total_successes <- 0
  total_failures <- 0



  ## batch processing ----
  for (batch_num in seq_along(batch_data$batch_indices))
    {
    batch_indices <- batch_data$batch_indices[[batch_num]]
    batch_texts <- texts[batch_indices]
    batch_ids <- ids[batch_indices]

    cli::cli_progress_message("Processing batch {batch_num}/{n_batches} ({length(batch_indices)} text{?s})")

    ## build batch requests ----
    requests <- oai_build_completions_request_list(
      inputs = batch_texts,
      model = model,
      temperature = temperature,
      max_tokens = max_tokens,
      schema = dumped_schema,
      system_prompt = system_prompt,
      key_name = key_name,
      endpoint_url = endpoint_url,
      max_retries = max_retries,
      timeout = timeout,
      endpointr_ids = batch_ids
    )

    # make sure we have some valid requests, or skip to the next iter
    is_valid_request <- purrr::map_lgl(requests, ~inherits(.x, "httr2_request"))
    valid_requests <- requests[is_valid_request]

    if (length(valid_requests) == 0) {
      cli::cli_alert_warning("No valid request{?s} in batch {batch_num}, skipping")
      next
    }

    # perform batch requests ----
    # get chunk_size individual responses and then handle them
    responses <- perform_requests_with_strategy(
      valid_requests,
      concurrent_requests = concurrent_requests,
      progress = TRUE
    )

    successes <- httr2::resps_successes(responses)
    failures <- httr2::resps_failures(responses)

    n_successes <- length(successes)
    n_failures <- length(failures)
    total_successes <- total_successes + n_successes
    total_failures <- total_failures + n_failures

    ## process batch responses ----
    # within batch results
    batch_results <- list()

    if (length(successes) > 0) {
      successes_ids <- purrr::map(successes, ~purrr::pluck(.x, "request", "headers", "endpointr_id")) |> unlist()
      successes_content <- purrr::map_chr(successes, .extract_successful_completion_content)

      batch_results$successes <- tibble::tibble(
        id = successes_ids,
        content = successes_content,
        .error = FALSE,
        .error_msg = NA_character_,
        .batch = batch_num
      )
    }

    if (length(failures) > 0) {
      failures_ids <- purrr::map(failures, ~purrr::pluck(.x, "request", "headers", "endpointr_id")) |> unlist()
      failures_msgs <- purrr::map_chr(failures, ~purrr::pluck(.x, "message", .default = "Unknown error"))

      batch_results$failures <- tibble::tibble(
        id = failures_ids,
        content = NA_character_,
        .error = TRUE,
        .error_msg = failures_msgs,
        .batch = batch_num
      )
    }

    batch_df <- dplyr::bind_rows(batch_results)

    # if(!is.null(output_file)){ # skip writing if output_file = NULL - can't be NULL after .handle_output_filename - as if NULL we write to tmp file
    if (nrow(batch_df) > 0) {
      if (batch_num == 1) {
        # if we're in the first batch write to csv with headers (col names)
        readr::write_csv(batch_df, output_file, append = FALSE)
      } else {
        # all other batches, append and don't use col names
        readr::write_csv(batch_df, output_file, append = TRUE, col_names = FALSE)
      }
    }
    # }

    cli::cli_alert_success("Batch {batch_num}: {n_successes} successful, {n_failures} failed")

    rm(requests, responses, successes, failures, batch_results, batch_df)
    gc(verbose = FALSE)
  }

  cli::cli_alert_success("Completed processing: {total_successes} successful, {total_failures} failed")

  # retrieve all results from the output file (results for all batches) - this may still be too inefficient, and should perhaps write to duckdb(?)
  final_results <- readr::read_csv(output_file, show_col_types = FALSE)

  return(final_results)
}

# oai_complete_df docs----
#' Process a data frame through OpenAI's Chat Completions API with chunked processing
#'
#' This function takes a data frame with text inputs and processes each row through
#' OpenAI's Chat Completions API using efficient chunked processing. It handles
#' concurrent requests, automatic retries, and structured output validation while
#' writing results progressively to disk.
#'
#' @details This function provides a data frame interface to the chunked processing
#' capabilities of `oai_complete_chunks()`. It extracts the specified text column,
#' processes texts in configurable chunks with concurrent API requests, and returns
#' results matched to the original data through the `id_var` parameter.
#'
#' The chunking approach enables processing of large data frames without memory
#' constraints. Results are written progressively to a CSV file (either specified
#' or auto-generated) and then read back as the return value.
#'
#' When using structured outputs with a `schema`, responses are validated against
#' the JSON schema and stored as JSON strings. Post-processing may be needed to
#' unnest these into separate columns.
#'
#' Failed requests are marked with `.error = TRUE` and include error messages,
#' allowing for easy filtering and retry logic on failures.
#'
#' @param df Data frame containing text to process
#' @param text_var Column name (unquoted) containing text inputs
#' @param id_var Column name (unquoted) for unique row identifiers
#' @inheritParams oai_complete_chunks
#'
#' @return A tibble with the original id column and additional columns:
#'   - `content`: API response content (text or JSON string if schema used)
#'   - `.error`: Logical indicating if request failed
#'   - `.error_msg`: Error message if failed, NA otherwise
#'   - `.batch`: Batch number for tracking
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage with a data frame
#' df <- tibble::tibble(
#'   doc_id = 1:3,
#'   text = c(
#'     "I absolutely loved this product!",
#'     "Terrible experience, would not recommend.",
#'     "It was okay, nothing special."
#'   )
#' )
#'
#' results <- oai_complete_df(
#'   df = df,
#'   text_var = text,
#'   id_var = doc_id,
#'   system_prompt = "Summarise the sentiment in one word."
#' )
#'
#' # Structured extraction with schema
#' sentiment_schema <- create_json_schema(
#'   name = "sentiment_analysis",
#'   schema = schema_object(
#'     sentiment = schema_string("positive, negative, or neutral"),
#'     confidence = schema_number("confidence score between 0 and 1"),
#'     required = list("sentiment", "confidence")
#'   )
#' )
#'
#' results <- oai_complete_df(
#'   df = df,
#'   text_var = text,
#'   id_var = doc_id,
#'   schema = sentiment_schema,
#'   temperature = 0
#' )
#'
#' # Post-process structured results
#' results |>
#'   dplyr::filter(!.error) |>
#'   dplyr::mutate(parsed = purrr::map(content, safely_from_json)) |>
#'   tidyr::unnest_wider(parsed)
#' }
#oai_complete_df docs----
oai_complete_df <- function(df,
                            text_var,
                            id_var,
                            model = "gpt-4.1-nano",
                            output_file = "auto",
                            system_prompt = NULL,
                            schema = NULL,
                            chunk_size = 1000,
                            concurrent_requests = 1L,
                            max_retries = 5L,
                            timeout = 30,
                            temperature = 0,
                            max_tokens = 500L,
                            key_name = "OPENAI_API_KEY",
                            endpoint_url = "https://api.openai.com/v1/chat/completions") {

  # input validation ----
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


  output_file <- .handle_output_filename(output_file, base_file_name = "oai_batch")

  text_vec <- dplyr::pull(df, !!text_sym)
  id_vec <- dplyr::pull(df, !!id_sym)

  results <- oai_complete_chunks(
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
    output_file = output_file
  )

  results <- dplyr::rename(results, !!id_sym := id)

  return(results)
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


#'should only pass in successes here, as we're not going to validate types here or try to catch errors and this is *marginally* quicker/memory efficient than pipe + pluck
#' @keywords internal
.extract_successful_completion_content <- function(resp) {
  httr2::resp_body_json(resp)$choices[[1]]$message$content
}


#' Safely extract JSON
#'
#' A wrapper around `jsonlite::fromJSON` that returns a list instead of
#' throwing an error when JSON parsing fails. Uses `purrr::possibly` to
#' provide graceful error handling. To be explicit: you will not receive an error
#' message if your JSON failed to parse, just an empty list. This property
#' allows you to handle NULL/NA results in the way you'd like to.
#'
#' @param ... Arguments passed to `jsonlite::fromJSON`. The first argument
#'   should be `txt` (a JSON string, URL or file to parse). Additional
#'   arguments include: `simplifyVector` (coerce JSON arrays containing only
#'   primitives to atomic vectors, default TRUE), `simplifyDataFrame` (coerce
#'   JSON arrays containing objects to data frames, default TRUE),
#'   `simplifyMatrix` (coerce JSON arrays containing equal-length sub-arrays
#'   to matrices, default TRUE), and `flatten` (automatically flatten nested
#'   data frames, default FALSE).
#'
#' @return Parsed JSON object on success, empty list on failure
#'
#' @examples
#' # Valid JSON
#' safely_from_json('{"name": "John", "age": 30}')
#'
#' # Invalid JSON returns empty list instead of error
#' safely_from_json('{"invalid": json}')
#'
#' # Works with URLs and files too
#' safely_from_json("https://api.example.com/data.json")
#'
#' @seealso [jsonlite::fromJSON()], [purrr::possibly()]
#' @export
safely_from_json <- purrr::possibly(.f = jsonlite::fromJSON,
                                     otherwise = list())

# TBD
# .validate_df_with_schema <- function(df, content_var, schema) {
#
#   return(list())
# }
