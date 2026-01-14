# Delete a File from the OpenAI Files API

Permanently deletes a file from the OpenAI Files API. This action cannot
be undone. Note that files associated with active batch jobs cannot be
deleted until the job completes.

## Usage

``` r
oai_file_delete(file_id, key_name = "OPENAI_API_KEY")
```

## Arguments

- file_id:

  File identifier (starts with 'file-'), returned by
  [`oai_batch_upload()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_upload.md)
  or
  [`oai_file_list()`](https://jpcompartir.github.io/EndpointR/reference/oai_file_list.md)

- key_name:

  Name of the environment variable containing your API key

## Value

A list containing the file id, object type, and deletion status (deleted
= TRUE/FALSE)

## See also

[`oai_file_list()`](https://jpcompartir.github.io/EndpointR/reference/oai_file_list.md)
to find file IDs,
[`oai_file_content()`](https://jpcompartir.github.io/EndpointR/reference/oai_file_content.md)
to retrieve file contents before deletion

## Examples

``` r
if (FALSE) { # \dontrun{
# Delete a specific file
result <- oai_file_delete("file-abc123")
result$deleted # TRUE if successful

} # }
```
