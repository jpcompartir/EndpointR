# Retrieve Content from a File on the OpenAI Files API

Downloads and returns the content of a file stored on the OpenAI Files
API. For batch job outputs, this returns JSONL content that can be
parsed with
[`oai_batch_parse_embeddings()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_parse_embeddings.md)
or
[`oai_batch_parse_completions()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_parse_completions.md).

## Usage

``` r
oai_file_content(file_id, key_name = "OPENAI_API_KEY")
```

## Arguments

- file_id:

  File identifier (starts with 'file-'), typically the output_file_id
  from
  [`oai_batch_status()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_status.md)

- key_name:

  Name of the environment variable containing your API key

## Value

A character string containing the file contents. For batch outputs, this
is JSONL format (one JSON object per line).

## See also

[`oai_batch_status()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_status.md)
to get output_file_id from completed batches,
[`oai_batch_parse_embeddings()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_parse_embeddings.md)
and
[`oai_batch_parse_completions()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_parse_completions.md)
to parse batch results

## Examples

``` r
if (FALSE) { # \dontrun{
# Get batch job status and download results
status <- oai_batch_status("batch_abc123")

if (status$status == "completed") {
  content <- oai_file_content(status$output_file_id)
  results <- oai_batch_parse_embeddings(content)
}
} # }
```
