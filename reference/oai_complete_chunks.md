# Process text chunks through OpenAI's Chat Completions API with batch file output

This function processes large volumes of text through OpenAI's Chat
Completions API in configurable chunks, writing results progressively to
parquet files. It handles concurrent requests, automatic retries, and
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
  output_dir = "auto",
  schema = NULL,
  concurrent_requests = 5L,
  temperature = 0L,
  max_tokens = 500L,
  max_retries = 5L,
  timeout = 30L,
  key_name = "OPENAI_API_KEY",
  endpoint_url = "https://api.openai.com/v1/chat/completions",
  id_col_name = "id"
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

- output_dir:

  Path to directory for the .parquet chunks. "auto" generates a
  timestamped directory name. If NULL, uses a temporary directory.

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

- id_col_name:

  Name for the ID column in output (default: "id"). When called from
  oai_complete_df(), this preserves the original column name.

## Value

A tibble containing all results with columns:

- ID column (name specified by `id_col_name`): Original identifier from
  input

- `content`: API response content (text or JSON string if schema used)

- `.error`: Logical indicating if request failed

- `.error_msg`: Error message if failed, NA otherwise

- `.chunk`: Chunk number for tracking

## Details

This function is designed for processing large text datasets that may
not fit comfortably in memory. It divides the input into chunks,
processes each chunk with concurrent API requests, and writes results
immediately to disk to minimise memory usage.

The function preserves data integrity by matching results to source
texts through the `ids` parameter. Each chunk is processed independently
with results written as parquet files to the output directory, allowing
for resumable processing if interrupted.

When using structured outputs with a `schema`, responses are validated
against the JSON schema but stored as raw JSON strings in the output
files. This allows for flexible post-processing without memory
constraints during the API calls.

The chunking strategy balances API efficiency with memory management.
Larger `chunk_size` values reduce overhead but increase memory usage.
Adjust based on your system resources and text sizes.

Avoid risk of data loss by setting a low-ish chunk_size (e.g. 5,000,
10,000). Each chunk is written to a `.parquet` file in the `output_dir=`
directory, which also contains a `metadata.json` file which tracks
important information such as the model and endpoint URL used. Be sure
to add output directories to .gitignore!

## Examples

``` r
if (FALSE) { # \dontrun{
# basic usage with automatic directory naming:
result <- oai_complete_chunks(
  texts = my_texts,
  ids = my_ids,
  model = "gpt-4.1-nano"
)

# large-scale processing with custom output directory:
result <- oai_complete_chunks(
  texts = my_texts,
  ids = my_ids,
  output_dir = "my_results",
  chunk_size = 10000
)

# structured extraction with schema:
result <- oai_complete_chunks(
  texts = my_texts,
  ids = my_ids,
  schema = my_schema,
  temperature = 0
)

# post-process structured results:
processed <- result |>
  dplyr::filter(!.error) |>
  dplyr::mutate(parsed = purrr::map(content, ~jsonlite::fromJSON(.x))) |>
  tidyr::unnest_wider(parsed)
} # }
```
