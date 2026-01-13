#' List Files on the OpenAI Files API
#'
#' Retrieve a list of files that have been uploaded to the OpenAI Files API,
#' filtered by purpose. Files are retained for 30 days after upload.
#'
#' @param purpose The intended purpose of the uploaded file. Must be one of
#'   "batch", "fine-tune", "assistants", "vision", "user_data", or "evals".
#' @param key_name Name of the environment variable containing your API key
#'
#' @returns A list containing file metadata and pagination information. Each
#'   file entry includes id, filename, purpose, bytes, created_at, and status.
#'
#' @export
#' @seealso [oai_file_content()] to retrieve file contents,
#'   [oai_file_delete()] to remove files,
#'   [oai_batch_file_upload()] to upload batch files
#' @examples
#' \dontrun{
#' # List all batch files
#' batch_files <- oai_file_list(purpose = "batch")
#'
#' # List fine-tuning files
#' ft_files <- oai_file_list(purpose = "fine-tune")
#'
#' # Access file IDs
#' file_ids <- purrr::map_chr(batch_files$data, "id")
#' }
oai_file_list <- function(purpose = c("batch", "fine-tune", "assistants", "vision", "user_data", "evals"), key_name = "OPENAI_API_KEY") {

  purpose <- match.arg(purpose)
  api_key <- get_api_key(key_name)

  httr2::request("https://api.openai.com/v1/files") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_url_query(purpose = purpose) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

}

#' Delete a File from the OpenAI Files API
#'
#' Permanently deletes a file from the OpenAI Files API. This action cannot
#' be undone. Note that files associated with active batch jobs cannot be
#' deleted until the job completes.
#'
#' @param file_id File identifier (starts with 'file-'), returned by
#'   [oai_batch_file_upload()] or [oai_file_list()]
#' @param key_name Name of the environment variable containing your API key
#'
#' @returns A list containing the file id, object type, and deletion status
#'   (deleted = TRUE/FALSE)
#'
#' @export
#' @seealso [oai_file_list()] to find file IDs,
#'   [oai_file_content()] to retrieve file contents before deletion
#' @examples
#' \dontrun{
#' # Delete a specific file
#' result <- oai_file_delete("file-abc123")
#' result$deleted # TRUE if successful
#'
#' }
oai_file_delete <- function(file_id, key_name = "OPENAI_API_KEY") {

  api_key <- get_api_key(key_name)

  httr2::request(paste0("https://api.openai.com/v1/files/", file_id)) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_method("DELETE") |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

#' Retrieve Content from a File on the OpenAI Files API
#'
#' Downloads and returns the content of a file stored on the OpenAI Files API.
#' For batch job outputs, this returns JSONL content that can be parsed with
#' [oai_batch_parse_embeddings()] or [oai_batch_parse_completions()].
#'
#' @param file_id File identifier (starts with 'file-'), typically the
#'   output_file_id from [oai_batch_status()]
#' @param key_name Name of the environment variable containing your API key
#'
#' @returns A character string containing the file contents. For batch outputs,
#'   this is JSONL format (one JSON object per line).
#'
#' @export
#' @seealso [oai_batch_status()] to get output_file_id from completed batches,
#'   [oai_batch_parse_embeddings()] and [oai_batch_parse_completions()] to
#'   parse batch results
#' @examples
#' \dontrun{
#' # Get batch job status and download results
#' status <- oai_batch_status("batch_abc123")
#'
#' if (status$status == "completed") {
#'   content <- oai_file_content(status$output_file_id)
#'   results <- oai_batch_parse_embeddings(content)
#' }
#' }
oai_file_content <- function(file_id, key_name = "OPENAI_API_KEY") {

  api_key <- get_api_key(key_name)

  resp <- httr2::request(paste0("https://api.openai.com/v1/files/", file_id, "/content")) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform()

  httr2::resp_body_string(resp)
}
