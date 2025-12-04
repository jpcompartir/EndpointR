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
  output_dir = "auto",
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

- output_dir:

  Path to directory for the .parquet chunks. "auto" generates a
  timestamped directory name. If NULL, uses a temporary directory.

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

- `.chunk`: Chunk number for tracking

## Details

This function provides a data frame interface to the chunked processing
capabilities of
[`oai_complete_chunks()`](https://jpcompartir.github.io/EndpointR/reference/oai_complete_chunks.md).
It extracts the specified text column, processes texts in configurable
chunks with concurrent API requests, and returns results matched to the
original data through the `id_var` parameter.

The chunking approach enables processing of large data frames without
memory constraints. Results are written progressively as parquet files
(either to a specified directory or auto-generated) and then read back
as the return value.

When using structured outputs with a `schema`, responses are validated
against the JSON schema and stored as JSON strings. Post-processing may
be needed to unnest these into separate columns.

Failed requests are marked with `.error = TRUE` and include error
messages, allowing for easy filtering and retry logic on failures.

Avoid risk of data loss by setting a low-ish chunk_size (e.g. 5,000,
10,000). Each chunk is written to a `.parquet` file in the `output_dir=`
directory, which also contains a `metadata.json` file. Be sure to add
output directories to .gitignore!

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with a data frame
df <- tibble::tibble(
  doc_id = 1:3,
  text = c(
    "I absolutely loved this product!",
    "Terrible experience, would not recommend.",
    "It was okay, nothing special."
  )
)

results <- oai_complete_df(
  df = df,
  text_var = text,
  id_var = doc_id,
  system_prompt = "Summarise the sentiment in one word."
)

# Structured extraction with schema
sentiment_schema <- create_json_schema(
  name = "sentiment_analysis",
  schema = schema_object(
    sentiment = schema_string("positive, negative, or neutral"),
    confidence = schema_number("confidence score between 0 and 1"),
    required = list("sentiment", "confidence")
  )
)

results <- oai_complete_df(
  df = df,
  text_var = text,
  id_var = doc_id,
  schema = sentiment_schema,
  temperature = 0
)

# Post-process structured results
results |>
  dplyr::filter(!.error) |>
  dplyr::mutate(parsed = purrr::map(content, safely_from_json)) |>
  tidyr::unnest_wider(parsed)
} # }
```
