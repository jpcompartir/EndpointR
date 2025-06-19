# Cannot input empty string
# Inputs (combined) cannot exceed 8092 tokens (max embed. length)
# Can input dimensions

oai_build_embedding_request <- function(input,
                                        model = "text-embedding-3-small",
                                        dimensions = NULL,
                                        max_retries = 5,
                                        timeout = 20,
                                        endpoint_url = "https://api.openai.com/v1/embeddings",
                                        key_name = "OPENAI_API_KEY") {

  api_key <- get_api_key(key_name)

  request <- base_request(endpoint_url = endpoint_url,
                          api_key = api_key)

  request <- request |>
    httr2::req_body_json(list(
      input = input,
      model = model,
      dimensions = dimensions,
      encoding_format = "float"
      )) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = max_retries,
                     backoff = ~ 2 ^ .x,
                     retry_on_failure = TRUE)


  return(request)

}

parse_oai_date <- function(date_string) {
  parsed_date <- as.POSIXct("Thu, 19 Jun 2025 09:28:47 GMT", format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  date <- as.date(parsed_date)
  return(date)
}

tidy_oai_embedding <- function(response_data) {

  # handles the single document case, or a batch of embeddings in a single response.

   purrr::map(response_data, ~{
    # input <- .x # we are *inside8 the map, so each object is an element of the list

    index_list <- .x$index
    embeddings_list <- .x$embedding


    col_names <- paste0("V", seq_along(embeddings_list))
    embeddings_list <- setNames(embeddings_list, col_names)

    index_df <- data.frame(oai_batch_id = index_list)
    embedding_df <- as.data.frame(embeddings_list)

    bind_cols(index_df, embedding_df) |>
      tibble::tibble()
  }) |>
    purrr::list_rbind()

}

# Checks that text is a length 1 vector, i.e. not batch
oai_embed_text <- function(text,
                           model = "text-embedding-3-small",
                           dimensions = NULL,
                           max_retries = 5,
                           timeout = 20,
                           endpoint_url = "https://api.openai.com/v1/embeddings",
                           key_name = "OPENAI_API_KEY") {

  stopifnot("Text must be a character vector" = is.character(text))

  request <- oai_build_embedding_request(
    input = text,
    model = model,
    dimensions = dimensions,
    max_retries = max_retries,
    timeout = timeout,
    endpoint_url = endpoint_url,
    key_name = key_name
  )

  response <- hf_perform_request(request)

  status <- response$status_code
  date <- parse_oai_date(response$headers$date)
  requests_remaining <- response$headers$`x-ratelimit-reset-requests`
  requests_reset_ms <- response$headers$`x-ratelimit-reset-requests`

  response_body_json <- httr2::resp_body_json(response) # object, data, model, usage

  response_body_json_data <- response_json$data  # top-level = list -> object, index, embedding.
  # fire this list into tidy_oai_embedding,



}

# Checks that texts is a length > 1 vector, i.e. not single text
oai_embed_batch <- function(texts,
                           model = "text-embedding-3-small",
                           dimensions = NULL,
                           batch_size = 1,
                           concurrent_requests = 1,
                           max_retries = 5,
                           timeout = 20,
                           endpoint_url = "https://api.openai.com/v1/embeddings",
                           key_name = "OPENAI_API_KEY") {

  # can take a list natively with the OpenAI embeddings.
  # data$embedding, data$index in the response if batch

}

oai_embed_df <- function(df,
                         text_var,
                         id_var,
                         model = "text-embedding-3-small",
                         dimensions = NULL,
                         batch_size = 1,
                         concurrent_requests = 1,
                         max_retries = 5,
                         timeout = 20,
                         endpoint_url = "https://api.openai.com/v1/embeddings",
                         key_name = "OPENAI_API_KEY" ) {

}
