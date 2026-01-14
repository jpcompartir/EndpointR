# Prepare and upload a file to be uploaded to the OpenAI Batch API

Prepare and upload a file to be uploaded to the OpenAI Batch API

## Usage

``` r
oai_batch_upload(
  jsonl_rows,
  purpose = c("batch", "fine-tune", "assistants", "vision", "user_data", "evals"),
  key_name = "OPENAI_API_KEY",
  endpoint_url = "https://api.openai.com/v1/files"
)
```

## Arguments

- jsonl_rows:

  Rows of valid JSON, output of an oai_batch_prepare\* function

- purpose:

  The intended purpose of the uploaded file. Must be one of "batch",
  "fine-tune", "assistants", "vision", "user_data", or "evals".

- key_name:

  Name of the environment variable containing your API key

- endpoint_url:

  OpenAI API endpoint URL (default: OpenAI's Files API V1)

## Value

Metadata for an upload to the OpenAI Files API

## See also

`oai_files_upload()`, `oai_files_list()`

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = c("doc_1", "doc_2"),
  text = c("Hello world", "Goodbye world")
)
jsonl_content <- oai_batch_prepare_embeddings(df, text_var = text, id_var = id)
file_info <- oai_batch_upload(jsonl_content)
file_info$id # Use this ID to create a batch job
} # }
```
