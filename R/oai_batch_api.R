# the batch

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



oai_batch_prepare_embeddings <- function(df, text_var, id_var, model = "text-embedding-3-small", dimensions, key_name = "OPENAI_API_KEY", endpoint_url = "https://api.openai.com/v1/embeddings") {

}

