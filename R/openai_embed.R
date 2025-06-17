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







}


oai_embed_text <- function(text,
                           model = "text-embedding-3-small",
                           dimensions = NULL,
                           max_retries = 5,
                           timeout = 20,
                           endpoint_url = "https://api.openai.com/v1/embeddings",
                           key_name = "OPENAI_API_KEY") {

}

oai_embed_batch <- function(texts,
                           model = "text-embedding-3-small",
                           dimensions = NULL,
                           batch_size = 1,
                           concurrent_requests = 1,
                           max_retries = 5,
                           timeout = 20,
                           endpoint_url = "https://api.openai.com/v1/embeddings",
                           key_name = "OPENAI_API_KEY") {

  # can take a list natively

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
