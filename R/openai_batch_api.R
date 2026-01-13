# embed request building ---- 
#' Create a single OpenAI Batch API - Embedding request
#'
#' This function prepares a single row of data for the OpenAI Batch/Files APIs, where each row should be valid JSON. The APIs do not guarantee the results will be in the same order, so we need to provide an ID with each request.
#' 
#' @param input Text input to embed
#' @param id A custom, unique row ID
#' @param model The embedding model to use
#' @param dimensions Number of embedding dimensions (NULL uses model default)
#' @param method The HTTP request type, usually 'POST'
#' @param encoding_format Data type of the embedding values
#' @param endpoint The API endpoint path, e.g. /v1/embeddings
#'
#' @returns a row of JSON
#'
#' @export
#' @examples
#' \dontrun{
#' text <- "embed_me"
#' id <- "id_1"
#' batch_req <- oai_batch_build_embed_req(text, id)
#' }
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
    
#' Prepare a Data Frame for the OpenAI Batch API - Embeddings
#'
#' @details Takes an entire data frame and turns each row into a valid line of JSON ready for a .jsonl file upload to the OpenAI Files API + Batch API job trigger.
#' 
#' Each request must have its own ID, as the Batch API makes no guarantees about the order the results will be returned in. 
#' 
#' To reduce the overall size, and the explanatory power of the Embeddings, you can set dimensions to lower than the default (which vary based on model). 
#' 
#' @param df A data frame containing text to process
#' @param text_var Name of the column containing input text
#' @param id_var Name of the column to use as row ID
#' @inheritParams oai_batch_build_embed_req
#'
#' @returns A list of JSON requests
#'
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = c("doc_1", "doc_2", "doc_3"),
#'   text = c("Hello world", "Embedding text", "Another document")
#' )
#' jsonl_content <- oai_batch_prepare_embeddings(df, text_var = text, id_var = id)
#' }
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
    

#' Create a Single OpenAI Batch API - Chat Completions Request
#'
#' This function prepares a single row of data for the OpenAI Batch/Files APIs,
#' where each row should be valid JSON. The APIs do not guarantee the results
#' will be in the same order, so we need to provide an ID with each request.
#'
#' @param input Text input (user message) for the completion
#' @param id A custom, unique row ID
#' @param model The chat completion model to use
#' @param system_prompt Optional system prompt to guide the model's behaviour
#' @param temperature Sampling temperature (0 = deterministic, higher = more random)
#' @param max_tokens Maximum number of tokens to generate
#' @param schema Optional JSON schema for structured output (json_schema object or list)
#' @param method The HTTP request type, usually 'POST'
#' @param endpoint The API endpoint path, e.g. /v1/chat/completions
#'
#' @returns A row of JSON suitable for the Batch API
#'
#' @export
#' @examples
#' \dontrun{
#' req <- oai_batch_build_completions_req(
#'   input = "What is the capital of France?",
#'   id = "query_1",
#'   model = "gpt-4o-mini",
#'   temperature = 0
#' )
#' }
oai_batch_build_completions_req <- function(input, id, model = "gpt-4o-mini", system_prompt = NULL, temperature = 0, max_tokens = 500L, schema = NULL, method = "POST", endpoint = "/v1/chat/completions") {
    
  messages <- list()
  
  if (!is.null(system_prompt)) {
    messages <- append(messages, list(list(role = "system", content = system_prompt)))
  }
  
  messages <- append(messages, list(list(role = "user", content = input)))
  
  body <- list(
    model = model,
    messages = messages,
    temperature = temperature,
    max_tokens = max_tokens
  )
  
  if (!is.null(schema)) {
    if (inherits(schema, "json_schema")) {
      body$response_format <- json_dump(schema)
    } else if (is.list(schema)) {
      body$response_format <- schema
    }
  }
  
  req_row <- list(
    custom_id = as.character(id),
    method = method,
    url = endpoint,
    body = body
  )
  
  jsonlite::toJSON(req_row, auto_unbox = TRUE)
}

#' Prepare a Data Frame for the OpenAI Batch API - Chat Completions
#'
#' @description Takes an entire data frame and turns each row into a valid line
#' of JSON ready for a .jsonl file upload to the OpenAI Files API + Batch API
#' job trigger.
#'
#' @details Each request must have its own ID, as the Batch API makes no
#' guarantees about the order the results will be returned in.
#'
#' @param df A data frame containing text to process
#' @param text_var Name of the column containing input text
#' @param id_var Name of the column to use as row ID
#' @inheritParams oai_batch_build_completions_req
#'
#' @returns A character string of newline-separated JSON requests
#'
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = c("q1", "q2"),
#'   prompt = c("What is 2+2?", "Explain gravity briefly.")
#' )
#' jsonl_content <- oai_batch_prepare_completions(
#'   df,
#'   text_var = prompt,
#'   id_var = id,
#'   system_prompt = "You are a helpful assistant."
#' )
#' }
oai_batch_prepare_completions <- function(df, text_var, id_var, model = "gpt-4o-mini", system_prompt = NULL, temperature = 0, max_tokens = 500L, schema = NULL, method = "POST", endpoint = "/v1/chat/completions") {
      
  text_sym <- rlang::ensym(text_var)
  id_sym <- rlang::ensym(id_var)
  
  .texts <- dplyr::pull(df, !!text_sym)
  .ids <- dplyr::pull(df, !!id_sym)
  
  if (!.validate_batch_inputs(.ids, .texts)) {
    return("")
  }
  
  ## pre-process schema once if S7 object to avoid repeated json_dump() calls
  if (!is.null(schema) && inherits(schema, "json_schema")) {
    schema <- json_dump(schema)
  }
  
  reqs <- purrr::map2_chr(.texts, .ids, \(x, y) {
    oai_batch_build_completion_req(
      input = x,
      id = as.character(y),
      model = model,
      system_prompt = system_prompt,
      temperature = temperature,
      max_tokens = max_tokens,
      schema = schema,
      method = method,
      endpoint = endpoint
    )
  })
  
  return(paste0(reqs, collapse = "\n"))
}
        

#' Prepare and upload a file to be uploaded to the OpenAI Batch API
#'
#' 
#' 
#' @param jsonl_rows Rows of valid JSON, output of an oai_batch_prepare* function
#' @param key_name Name of the environment variable containing your API key
#' @param purpose File purpose tag, e.g. 'batch', 'fine-tune'
#'
#' @returns Metadata for an upload to the OpenAI Files API
#'
#' @export
#' @seealso `oai_files_upload()`, `oai_files_list()`
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = c("doc_1", "doc_2"),
#'   text = c("Hello world", "Goodbye world")
#' )
#' jsonl_content <- oai_batch_prepare_embeddings(df, text_var = text, id_var = id)
#' file_info <- oai_batch_file_upload(jsonl_content)
#' file_info$id # Use this ID to create a batch job
#' }
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
#' Trigger a batch job to run on an uploaded file
#'
#' @details Once a file has been uploaded to the OpenAI Files API it's necessary to trigger the batch job. This will ensure that your file is processed, and processing is finalised within the 24 hour guarantee.
#' 
#' It's important to choose the right endpoint. If processing should be done by the Completions API, be sure to route to v1/chat/completions, and this must match each row in your uploaded file.
#' 
#' Batch Job Ids start with "batch_", you'll receive a warning if you try to check batch status on a Files API file (the Files/Batch API set up is a lil bit clumsy for me)
#' 
#' @param file_id File ID returned by oai_batch_file_upload()
#' @param endpoint The API endpoint path, e.g. /v1/embeddings
#' @param completion_window Time window for batch completion (OpenAI guarantees 24h only)
#' @param metadata Optional list of metadata to tag the batch with
#' @inheritParams oai_batch_file_upload
#'
#' @returns Metadata about an OpenAI Batch Job Including the batch ID
#'
#' @export
#' @examples
#' \dontrun{
#' # After uploading a file with oai_batch_file_upload()
#' batch_job <- oai_batch_create(
#'   file_id = "file-abc123",
#'   endpoint = "/v1/embeddings"
#' )
#' batch_job$id # Use this to check status later
#' }
oai_batch_create <- function(file_id, endpoint = c("/v1/embeddings", "/v1/chat/completions"), completion_window = "24h", metadata = NULL, key_name = "OPENAI_API_KEY") {
  
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
          
#' Check the Status of a Batch Job on the OpenAI Batch API
#'
#' @param batch_id Batch identifier (starts with 'batch_'), returned by oai_batch_create()
#' @inheritParams oai_batch_file_upload
#'
#' @returns Metadata about an OpenAI Batch API Job, including status, error_file_id, output_file_id, input_file_id etc.
#'
#' @export
#' @examples
#' \dontrun{
#' status <- oai_batch_status("batch_abc123")
#' status$status # e.g., "completed", "in_progress", "failed"
#' status$output_file_id # File ID for results when completed
#' }
oai_batch_status <- function(batch_id, key_name = "OPENAI_API_KEY") {
  
  api_key <- get_api_key(key_name)
  
  httr2::request(paste0("https://api.openai.com/v1/batches/", batch_id)) |>
  httr2::req_auth_bearer_token(api_key) |>
  httr2::req_error(is_error = ~ FALSE) |>
  httr2::req_perform() |>
  httr2::resp_body_json()
}
          
#' List Batch Jobs on the OpenAI Batch API
#'
#' Retrieve a paginated list of batch jobs associated with your API key.
#'
#' @param limit Maximum number of batch jobs to return
#' @param after Cursor for pagination; batch ID to start after
#' @inheritParams oai_batch_file_upload
#'
#' @returns A list containing batch job metadata and pagination information
#'
#' @export
#' @examples
#' \dontrun{
#' # List recent batch jobs
#' batches <- oai_batch_list(limit = 10)
#'
#' # Paginate through results
#' next_page <- oai_batch_list(after = batches$last_id)
#' }
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
          
#' Cancel a Running Batch Job on the OpenAI Batch API
#'
#' Cancels an in-progress batch job. The batch will stop processing new
#' requests, but requests already being processed may still complete.
#'
#' @inheritParams oai_batch_status
#' @inheritParams oai_batch_file_upload
#'
#' @returns Metadata about the cancelled batch job
#'
#' @export
#' @examples
#' \dontrun{
#' # Cancel a batch job that's taking too long
#' cancelled <- oai_batch_cancel("batch_abc123")
#' cancelled$status # Will be "cancelling" or "cancelled"
#' }
oai_batch_cancel <- function(batch_id, key_name = "OPENAI_API_KEY") {
  
  api_key <- get_api_key(key_name)
  
  httr2::request(paste0("https://api.openai.com/v1/batches/", batch_id, "/cancel")) |>
  httr2::req_auth_bearer_token(api_key) |>
  httr2::req_method("POST") |>
  httr2::req_error(is_error = ~ FALSE) |>
  httr2::req_perform() |>
  httr2::resp_body_json()
}
          
          
# results parsing ----
#' Parse an Embeddings Batch Job into a Data Frame
#'
#' Parses the JSONL content returned from a completed embeddings batch job
#' and converts it into a tidy data frame with one row per embedding.
#'
#' @param content Character string of JSONL content from the batch output file
#' @param original_df Optional original data frame to rename custom_id column
#' @param id_var If original_df provided, the column name to rename custom_id to
#'
#' @returns A tibble with custom_id (or renamed), .error, .error_msg, and
#'   embedding dimensions (V1, V2, ..., Vn)
#'
#' @export
#' @examples
#' \dontrun{
#' # After downloading batch results with oai_files_content()
#' content <- oai_files_content(status$output_file_id)
#' embeddings_df <- oai_batch_parse_embeddings(content)
#'
#' # Optionally rename the ID column to match original data
#' embeddings_df <- oai_batch_parse_embeddings(
#'   content,
#'   original_df = my_df,
#'   id_var = doc_id
#' )
#' }
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
    
    embedding <- purrr::pluck(item, "response", "body", "data", 1, "embedding",.default = NULL)
    
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
          
#' Parse a Completions Batch Job into a Data Frame
#'
#' Parses the JSONL content returned from a completed chat completions batch
#' job and converts it into a tidy data frame with one row per response.
#'
#' @inheritParams oai_batch_parse_embeddings
#'
#' @returns A tibble with custom_id (or renamed), content, .error, and .error_msg
#'
#' @export
#' @examples
#' \dontrun{
#' # After downloading batch results with oai_files_content()
#' content <- oai_files_content(status$output_file_id)
#' completions_df <- oai_batch_parse_completions(content)
#'
#' # Optionally rename the ID column to match original data
#' completions_df <- oai_batch_parse_completions(
#'   content,
#'   original_df = my_df,
#'   id_var = query_id
#' )
#' }
oai_batch_parse_completions <- function(content, original_df = NULL, id_var = NULL) {
  
  lines <- strsplit(content, "\n")[[1]]
  lines <- lines[nchar(lines) > 0]
  
  if (length(lines) == 0) {
    return(tibble::tibble(
      custom_id = character(),
      content = character(),
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
        content = NA_character_,
        .error = TRUE,
        .error_msg = item$error$message %||% "Unknown error"
      ))
    }
    
    response_content <- purrr::pluck(
      item, "response", "body", "choices", 1, "message", "content",
      .default = NA_character_
    )
    
    tibble::tibble(
      custom_id = custom_id,
      content = response_content,
      .error = FALSE,
      .error_msg = NA_character_
    )
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
          
          
# internal/helpers ----
#' @keywords internal
.validate_batch_inputs <- function(.ids,  .texts,  max_requests = 50000) { 
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