# List Files on the OpenAI Files API

Retrieve a list of files that have been uploaded to the OpenAI Files
API, filtered by purpose. Files are retained for 30 days after upload.

## Usage

``` r
oai_file_list(
  purpose = c("batch", "fine-tune", "assistants", "vision", "user_data", "evals"),
  key_name = "OPENAI_API_KEY"
)
```

## Arguments

- purpose:

  The intended purpose of the uploaded file. Must be one of "batch",
  "fine-tune", "assistants", "vision", "user_data", or "evals".

- key_name:

  Name of the environment variable containing your API key

## Value

A list containing file metadata and pagination information. Each file
entry includes id, filename, purpose, bytes, created_at, and status.

## See also

[`oai_file_content()`](https://jpcompartir.github.io/EndpointR/reference/oai_file_content.md)
to retrieve file contents,
[`oai_file_delete()`](https://jpcompartir.github.io/EndpointR/reference/oai_file_delete.md)
to remove files,
[`oai_batch_upload()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_upload.md)
to upload batch files

## Examples

``` r
if (FALSE) { # \dontrun{
# List all batch files
batch_files <- oai_file_list(purpose = "batch")

# List fine-tuning files
ft_files <- oai_file_list(purpose = "fine-tune")

# Access file IDs
file_ids <- purrr::map_chr(batch_files$data, "id")
} # }
```
