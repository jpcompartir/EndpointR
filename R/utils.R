#' Retrieve an API key which has been stored as an Environment Variable.
#'
#' @description
#' Retrieve an API key from .Renviron. The API key should have been set via
#' `set_api_key()`or manually placed in your .Renviron file.
#'
#' @param key_name The name of the API in the format "ENDPOINT_API_KEY" -> "ANTHROPIC_API_KEY"
#'
#' @returns character
#' @export
#'
#' @examples
#' \dontrun{
#'   # retrieve an Anthropic API key
#'   anthropic_key <- get_api_key("ANTHROPIC_API_KEY")
#'
#'   # use the key in a function call
#'   my_function(api_key = anthropic_key)
#' }
get_api_key <- function(key_name) {

  stopifnot("`key_name` should be a string" = is.character(key_name)) # add && API_KEY check here?

  renviron_path <- path.expand("~/.Renviron")
  if(!file.exists(renviron_path)){
    cli::cli_abort("{.file {renviron_path}} not found, please create one.")
  }

  if(!endsWith(key_name, "API_KEY")){
    cli::cli_abort("{.val {key_name}} is an invalid name. API keys must end with 'API_KEY'")
  }

  api_key <- Sys.getenv(key_name)

  if(identical(api_key, "")) {
    cli::cli_abort("{.val {key_name}} was not found, please set with {.code set_api_key({key_name})}, and restart your R session for changes to take effect.")
  }

  return(api_key)
}


#' @title Set your API keys so they can be accessed by EndpointR
#'
#' @description
#' Set an API key for each endpoint - where endpoint could be Anthropic, OpenAI,
#' a specific Hugging Face Inference endpoint, or another supported provider. Add overwrite=TRUE if you need to update an existing key. You will then be able to retrieve the key with the `get_api_key()` function.
#'
#' @param key_name The name of the API in the format "ENDPOINT_API_KEY" -> "ANTHROPIC_API_KEY"
#' @param overwrite Whether to overwrite an existing value for the API key.
#'
#' @returns Nothing
#' @export
#'
#' @examples
#' \dontrun{
#'   # set an Anthropic API key
#'   set_api_key("ANTHROPIC_API_KEY")
#'
#'   # update an existing OpenAI key
#'   set_api_key("OPENAI_API_KEY", overwrite = TRUE)
#' }
set_api_key <- function(key_name, overwrite = FALSE) {

  if(!endsWith(key_name, "API_KEY")) {
    cli::cli_abort(c(
      "{.field api_key} must end with 'API_KEY'",
      "x" = "ANTHROPIC_KEY",
      "v" = "ANTHROPIC_API_KEY"
    )
    )
  }

  # Process:
  # Check for a .Renviron file, create one if it doesn't exist. Notify user via CLI where the file is
  # Use askpass to get the key's value so the user doesn't have to type it into their console (for security reasons)
  # Check the key was entered, then check whether one exists in the .Renviron file, if it does then notify the user - unless they selected overwrite = TRUE, then just overwrite.
  # Let the user know where the key is, and how to load it.

  renviron_path <- path.expand("~/.Renviron")
  if(!file.exists(renviron_path)){
    file.create(renviron_path)
    cli::cli_alert_success("Created a .Renviron file at {.file {renviron_path}}")
  }

  api_key_value <- askpass::askpass(glue::glue("Please enter your {key_name}"))

  if(identical(api_key_value, "")) {
    cli::cli_abort("No value provided for {.val {key_name}}, please try again.")
  }

  renviron_contents <- readLines(renviron_path)
  key_already_exists <- any(
    grepl(glue::glue("^{key_name}"), renviron_contents)
    )

  if(key_already_exists && !overwrite){
    cli::cli_abort(message = c("x" = "Key already exists, set {.code overwrite=TRUE} to overwrite {.val {key_name}}"))
  }

  key_value_pair <- glue::glue("{key_name}={api_key_value}")

  # when key is already set and overwrite = TRUE - already exited if key was set and overwrite was not TRUE
  if(key_already_exists) {
    renviron_contents[grep(glue::glue("^{key_name}="), renviron_contents)] <- key_value_pair
    writeLines(renviron_contents, renviron_path) # re-write the whole file, replacing existing key
    cli::cli_alert_success("{.val {key_name}} was updated in {.file {renviron_path}}")
  } else {
    # now we need to *append* to the .Renvion file, so we cat with append = TRUE
    cat(key_value_pair, file = renviron_path, append = TRUE, sep = "\n")
    cli::cli_alert_success(c("{.val {key_name}} was added to {.file {renviron_path}}"))
  }

  cli::cli_alert_info("Restart your R session for changes to take effect, then call {.code get_api_key({key_name})}")

  invisible(TRUE)
}


#' Validate that a Hugging Face Inference Endpoint is available
#'
#' @description
#' Checks if an endpoint URL is valid and accessible with the provided API key.
#' This function sends a small test request to verify the endpoint works.
#'
#' @param endpoint_url The URL of the Hugging Face Inference API endpoint
#' @param key_name Name of the environment variable containing the API key
#'
#' @return logical TRUE if endpoint is valid, otherwise stops with an error
#' @export
#'
#' @examples
#' \dontrun{
#'   # Validate endpoint retrieving API key from environment
#'   validate_hf_endpoint(
#'     endpoint_url = "https://my-endpoint.huggingface.cloud",
#'     key_name = "HF_API_KEY"
#'   )
#'
#'   # Using default key name
#'   validate_hf_endpoint("https://my-endpoint.huggingface.cloud")
#' }
validate_hf_endpoint <- function(endpoint_url, key_name) {
  stopifnot(
    "endpoint_url must be provided" = !is.null(endpoint_url) && nchar(endpoint_url) > 0,
    "endpoint_url must be a character string" = is.character(endpoint_url)
  )

  api_key <- get_api_key(key_name)
  test_text <- "Hello world"

  req <- httr2::request(endpoint_url) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_method("POST") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_bearer_token(token = api_key) |>
    httr2::req_body_json(list(inputs = test_text)) |>
    httr2::req_timeout(10) # short timeout to fail fast

  # Handle both HTTP errors and connection issues
  tryCatch({
    resp <- httr2::req_perform(req)

    # Check if status code indicates an error (400 or above)
    if (httr2::resp_status(resp) >= 400) {
      cli::cli_abort("Endpoint returned error: {httr2::resp_status_desc(resp)}")
    }

    return(TRUE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Cannot connect to Hugging Face endpoint",
      "i" = "URL: {endpoint_url}",
      "x" = "Error: {conditionMessage(e)}"
    ))
    return(FALSE)
  })
}

# chunk_dataframe docs ----
#' Split a data frame into chunks for batch processing
#'
#' @description
#' Splits a data frame into chunks of specified size.

#'
#' @param df A data frame to split into batches
#' @param chunk_size Number of rows per batch
#'
#' @return A list of data frames, each with at most chunk_size rows
#' @keywords internal
# chunk_dataframe docs ----
chunk_dataframe <- function(df, chunk_size) {
  stopifnot(
    "df must be a data frame" = is.data.frame(df),
    "chunk_size must be a positive integer" = is.numeric(chunk_size) && chunk_size > 0
  )

  # don't batch if the df is smaller than batch size
  if (nrow(df) <= chunk_size) {
    return(list(df))
  }

  df_chunks <- split(df, ceiling(seq_len(nrow(df)) / chunk_size))
  return(df_chunks)
}


batch_vector <- function(vector, batch_size) {
  stopifnot("`batch_vector` requires a vector as input" = is.vector(vector),
            "`batch_vector requires a non-empty vector as input" = length(vector) >0)

  batch_indices <-  split(seq_along(vector), ceiling(seq_along(vector) / batch_size))
  batch_inputs <- purrr::map(batch_indices, ~vector[.x])


  return(
    list(batch_indices = batch_indices,
         batch_inputs = batch_inputs)
  )
}


extract_field <- function(api_response, field_name) {
  recursive_map_collect <- function(x, field) {
    if (is.list(x)) {
      # if named list and has the field, collect it
      if (!is.null(names(x)) && field %in% names(x)) {
        return(c(list(x[[field]]), unlist(purrr::map(x, ~recursive_map_collect(., field)), recursive = FALSE)))
      } else {
        # check all elements
        unlist(purrr::map(x, ~recursive_map_collect(., field)), recursive = FALSE)
      }
    } else {
      list() # base case - not a list
    }
  }

  purrr::compact(recursive_map_collect(api_response, field_name))
}


#' @keywords internal
.handle_output_filename <- function(x, base_file_name = "batch_processing_") {
  if (is.null(x)) {
    return(tempfile(pattern = base_file_name, fileext = ".csv"))
  }

  if(identical(x, "auto")) {
    timestamp <- format(Sys.time(), "%d%m%Y_%H%M%S")
    output_file <- paste0(base_file_name, "_", timestamp, ".csv")

    return(output_file)
  }

  if (!endsWith(tolower(x), ".csv")) {
    cli::cli_abort("`output_file` must have a .csv extension")
  }

  return(x)
}

# modifying the .handle_output_filename to work with .parquet
#' @keywords internal
.handle_output_directory <- function(x, base_dir_name = "batch_processing_") {
  if (is.null(x)) {
    return(tempfile(pattern = base_dir_name))
  }

  if(identical(x, "auto")) {
    timestamp <- format(Sys.time(), "%d%m%Y_%H%M%S")
    output_dir <- glue::glue("{base_dir_name}_{timestamp}")
    return(output_dir)
  }

  # Accept directory path directly
  return(x)
}


parse_oai_date <- function(date_string) {
  parsed_date <- as.POSIXct(date_string, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  date <- as.Date(parsed_date)
  return(date)
}



#' Check the max number of tokens allowed for your inputs
#'
#' This function requires the model to have 'tokenizer_config.json' file with a
#' `model_max_length` key, otherwise it will error.
#'
#' @param model_name name of the model e.g. 'sentence-transformers/mpnet-base-v2'
#' @param api_key Your Hugging Face auth token
#'
#' @returns Integer value of the model_max_length from tokenizer config
#' @export
#'
hf_get_model_max_length <- function(model_name, api_key = "HF_API_KEY") {
  config_url <- glue::glue("https://huggingface.co/{model_name}/resolve/main/tokenizer_config.json")

  use_api_key <- get_api_key(api_key)

  req <- httr2::request(config_url)

  if (!is.null(use_api_key)) {
    req <- req |>
      httr2::req_headers(Authorization = paste("Bearer", use_api_key))
  }

  response <- req |> httr2::req_perform()

  tokenizer_config <- response |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()

  return(tokenizer_config$model_max_length)
}


#' Retrieve information about an endpoint
#'
#' @param endpoint_url Hugging Face Embedding Endpoint
#' @param key_name Name of environment variable containing the API key (default: "HF_API_KEY")
#'
#' @returns JSON of endpoint information
#' @export
#'
hf_get_endpoint_info <- function(endpoint_url, key_name = "HF_API_KEY") {

  info_endpoint_url <- glue::glue("{endpoint_url}/info")
  api_key = get_api_key(key_name)

  info <-httr2::request(info_endpoint_url) |>
    httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  return(info)
}
