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

oai_batch_prepare_embeddings <- function(df, text_var, id_var, model = "text-embedding-3-small", dimensions = NULL) {


}

oai_batch_file_upload <- function(jsonl_rows, key_name = "OPENAI_API_KEY") {

  api_key <- get_api_key(key_name)

  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp)) # if session crashes we drop the file from mem safely
  writeLines(jsonl_rows, tmp) # send the content to the temp file for uploading to OAI
  # question here is whether to also save this somewhere by force...
  # once OAI have the file it's backed up for 30 days.

  httr2::request(base_url = "https://api.openai.com/v1/files") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_multipart(file = curl::form_file(tmp),
                              purpose = "batch") |>
    httr2::req_perform() |>
    httr2::resp_body_json()





}
