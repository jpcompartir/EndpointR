# Process text chunks through OpenAI's Chat Completions API with batch file output

This function processes large volumes of text through OpenAI's Chat
Completions API in configurable chunks, writing results progressively to
a CSV file. It handles concurrent requests, automatic retries, and
structured outputs while managing memory efficiently for large-scale
processing.

## Usage

``` r
oai_complete_chunks(
  texts,
  ids,
  chunk_size = 5000L,
  model = "gpt-4.1-nano",
  system_prompt = NULL,
  output_file = "auto",
  schema = NULL,
  concurrent_requests = 5L,
  temperature = 0L,
  max_tokens = 500L,
  max_retries = 5L,
  timeout = 30L,
  key_name = "OPENAI_API_KEY",
  endpoint_url = "https://api.openai.com/v1/chat/completions"
)
```

## Arguments

- texts:

  Character vector of texts to process

- ids:

  Vector of unique identifiers corresponding to each text (same length
  as texts)

- chunk_size:

  Number of texts to process in each batch (default: 5000)

- model:

  OpenAI model to use (default: "gpt-4.1-nano")

- system_prompt:

  Optional system prompt applied to all requests

- output_file:

  Path to .CSV file for results. "auto" generates the filename, location
  and is persistent across sessions. If NULL, generates timestamped
  filename.

- schema:

  Optional JSON schema for structured output (json_schema object or
  list)

- concurrent_requests:

  Integer; number of concurrent requests (default: 5)

- temperature:

  Sampling temperature (0-2), lower = more deterministic (default: 0)

- max_tokens:

  Maximum tokens per response (default: 500)

- max_retries:

  Maximum retry attempts per failed request (default: 5)

- timeout:

  Request timeout in seconds (default: 30)

- key_name:

  Name of environment variable containing the API key (default:
  OPENAI_API_KEY)

- endpoint_url:

  OpenAI API endpoint URL

## Value

A tibble containing all results with columns:

- `id`: Original identifier from input

- `content`: API response content (text or JSON string if schema used)

- `.error`: Logical indicating if request failed

- `.error_msg`: Error message if failed, NA otherwise

- `.batch`: Batch number for tracking

## Details

This function is designed for processing large text datasets that may
not fit comfortably in memory. It divides the input into chunks,
processes each chunk with concurrent API requests, and writes results
immediately to disk to minimise memory usage.

The function preserves data integrity by matching results to source
texts through the `ids` parameter. Each chunk is processed independently
with results appended to the output file, allowing for resumable
processing if interrupted.

When using structured outputs with a `schema`, responses are validated
against the JSON schema but stored as raw JSON strings in the output
file. This allows for flexible post-processing without memory
constraints during the API calls.

The chunking strategy balances API efficiency with memory management.
Larger `chunk_size` values reduce overhead but increase memory usage.
Adjust based on your system resources and text sizes.

## Examples

``` r
if (FALSE) { # \dontrun{
# basic usage with automatic file naming:

# large-scale processing with custom output file:
#structured extraction with schema:


# post-process structured results:
xx <- xx |>
  dplyr::filter(!.error) |>
  dplyr::mutate(parsed = map(content, ~jsonlite::fromJSON(.x))) |>
  unnest_wider(parsed)
} # }
```
