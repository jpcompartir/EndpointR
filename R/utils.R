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


#' Validate a Hugging Face Inference Endpoint
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
