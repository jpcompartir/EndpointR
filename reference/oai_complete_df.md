# Process a data frame through OpenAI's Chat Completions API with chunked processing

This function takes a data frame with text inputs and processes each row
through OpenAI's Chat Completions API using efficient chunked
processing. It handles concurrent requests, automatic retries, and
structured output validation while writing results progressively to
disk.

## Usage

``` r
oai_complete_df(
  df,
  text_var,
  id_var,
  model = "gpt-4.1-nano",
  output_file = "auto",
  system_prompt = NULL,
  schema = NULL,
  chunk_size = 1000,
  concurrent_requests = 1L,
  max_retries = 5L,
  timeout = 30,
  temperature = 0,
  max_tokens = 500L,
  key_name = "OPENAI_API_KEY",
  endpoint_url = "https://api.openai.com/v1/chat/completions"
)
```

## Arguments

- df:

  Data frame containing text to process

- text_var:

  Column name (unquoted) containing text inputs

- id_var:

  Column name (unquoted) for unique row identifiers

- model:

  OpenAI model to use (default: "gpt-4.1-nano")

- output_file:

  Path to .CSV file for results. "auto" generates the filename, location
  and is persistent across sessions. If NULL, generates timestamped
  filename.

- system_prompt:

  Optional system prompt applied to all requests

- schema:

  Optional JSON schema for structured output (json_schema object or
  list)

- chunk_size:

  Number of texts to process in each batch (default: 5000)

- concurrent_requests:

  Integer; number of concurrent requests (default: 5)

- max_retries:

  Maximum retry attempts per failed request (default: 5)

- timeout:

  Request timeout in seconds (default: 30)

- temperature:

  Sampling temperature (0-2), lower = more deterministic (default: 0)

- max_tokens:

  Maximum tokens per response (default: 500)

- key_name:

  Name of environment variable containing the API key (default:
  OPENAI_API_KEY)

- endpoint_url:

  OpenAI API endpoint URL

## Value

A tibble with the original id column and additional columns:

- `content`: API response content (text or JSON string if schema used)

- `.error`: Logical indicating if request failed

- `.error_msg`: Error message if failed, NA otherwise

- `.batch`: Batch number for tracking

## Details

This function provides a data frame interface to the chunked processing
capabilities of
[`oai_complete_chunks()`](https://jpcompartir.github.io/EndpointR/reference/oai_complete_chunks.md).
It extracts the specified text column, processes texts in configurable
chunks with concurrent API requests, and returns results matched to the
original data through the `id_var` parameter.

The chunking approach enables processing of large data frames without
memory constraints. Results are written progressively to a CSV file
(either specified or auto-generated) and then read back as the return
value.

When using structured outputs with a `schema`, responses are validated
against the JSON schema and stored as JSON strings. Post-processing may
be needed to unnest these into separate columns.

Failed requests are marked with `.error = TRUE` and include error
messages, allowing for easy filtering and retry logic on failures.

## Examples

``` r
if (FALSE) { # \dontrun{

} # }
```
