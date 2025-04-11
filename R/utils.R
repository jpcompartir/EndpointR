#' Retrieve an API key which has been stored as an Environment Variable.
#'
#' @param endpoint
#'
#' @returns
#' @export
#'
#' @examples
get_api_key <- function(endpoint) {

  stopifnot(is.character(endpoint)) # add && API_KEY check here?

  # thinking I probably shouldn't do this for the user, and they should set it with:
  # ENDPOINT_API_KEY in all caps. If that's documented then it should really be the user's decision
  # because we don't want weird surprises later when the user tries to retrieve their key.
  # we should retrieve what they input when they ask for it, I think...
  if(!grepl("_API_KEY", endpoint)){
    env_variable <- paste0(toupper(endpoint), "_API_KEY")
  }

  api_key <- Sys.getenv(env_variable)

  if(identical(api_key, "")) {
    stop(paste0("`", env_variable, "` not found, please set with `set_api_key_as_env()` or manually edit your `.Renviron` file."))
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
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#'   # Set an Anthropic API key
#'   set_api_key("ANTHROPIC_API_KEY")
#'
#'   # Update an existing OpenAI key
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
    grepl(glue::glue("^{key_name}"), renviron_path)
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
