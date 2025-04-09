#' Title
#'
#' @param endpoint
#'
#' @returns
#' @export
#'
#' @examples
get_api_key_from_env <- function(endpoint) {

  stopifnot(is.character(endpoint))

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
#' a specific Hugging Face Inference endpoint, or another supported provider.
#' EndpointR will try to extract the correct API keys from your

#' @details
#' EndpointR will add the key to your .Renviron file, and append "_API_KEY". You
#' should enter the name of the service, without "_API_KEY" appended. For Anthropic,
#' input 'ANTHROPIC' as the argument
#'
#' @param endpoint
#'
#' @returns
#' @export
#'
#' @importFrom rlang `:=`function
#' @examples
set_api_key_as_env <- function(endpoint, key, overwrite = FALSE) {

  # 1. locate the .Renviron file
  # 2. convert the value of endpoint to an _API_KEY fit for storing as an environment variable
  # 3. Check the environment variable isn't already set
  #   3.1 if it wasn't, write it to the file
  # 4 check the variable is accessible
  # 4. Return the API key (??) - seems bad - or set the tmeporary environment variable?

  renviron_path <- path.expand("~/.Renviron")
  renviron_exists <- file.exists(renviron_path)

  if(!grepl("_API_KEY", endpoint)){
    env_variable <- paste0(toupper(endpoint), "_API_KEY")
  } else {
    env_variable <- endpoint
  }

  if(renviron_exists){
    renviron_content <- readLines(renviron_path)

    if(any(grepl(env_variable, renviron_content))) {
      if(overwrite){

      } else {
        cat(paste0(env_variable, "=",key),file = renviron_path, append = TRUE, sep = "\n")
      }
      cli::cli_alert_info("{env_variable} has already been set.")
      invisible(TRUE)
    } else {
      cat(paste0(env_variable, "=",key),file = renviron_path, append = TRUE, sep = "\n")
      cli::cli_alert_success("{env_variable} set, restart your R session for changes to take effect.")
    }
 }

  invisible(TRUE)
}
