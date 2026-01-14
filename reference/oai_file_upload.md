# Upload a file to the OpenAI Files API

Upload a file to the OpenAI Files API

## Usage

``` r
oai_file_upload(
  file,
  purpose = c("batch", "fine-tune", "assistants", "vision", "user_data", "evals"),
  key_name = "OPENAI_API_KEY",
  endpoint_url = "https://api.openai.com/v1/files"
)
```

## Arguments

- file:

  File object you wish to upload

- purpose:

  The intended purpose of the uploaded file. Must be one of "batch",
  "fine-tune", "assistants", "vision", "user_data", or "evals".

- key_name:

  Name of the environment variable containing your API key

- endpoint_url:

  OpenAI API endpoint URL (default: OpenAI's Files API V1)

## Value

File upload status and metadata inlcuding id, purpose, filename,
created_at etc.

## See also

<https://platform.openai.com/docs/api-reference/files?lang=curl>

## Examples

``` r
if (FALSE) { # \dontrun{
 tmp <- tempfile(fileext = ".jsonl")
 writeLines("Hello!", tmp)
 oai_file_upload(
   file = tmp,
   purpose = "user_data"
)

} # }
```
