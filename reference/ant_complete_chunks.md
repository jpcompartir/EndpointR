# Process text chunks through Anthropic's Messages API with batch file output

Processes large volumes of text through Anthropic's Messages API in
configurable chunks, writing results progressively to parquet files.
Handles concurrent requests, automatic retries, and structured outputs.

## Usage

``` r
ant_complete_chunks(
  texts,
  ids,
  chunk_size = 5000L,
  model = "claude-haiku-4-5",
  system_prompt = NULL,
  output_dir = "auto",
  overwrite = FALSE,
  schema = NULL,
  concurrent_requests = 5L,
  temperature = 0,
  max_tokens = 1024L,
  max_retries = 5L,
  timeout = 30L,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_MESSAGES_ENDPOINT,
  id_col_name = "id",
  effort = NULL
)
```

## Arguments

- texts:

  Character vector of texts to process

- ids:

  Vector of unique identifiers (same length as texts)

- chunk_size:

  Number of texts per chunk before writing to disk

- model:

  Anthropic model to use

- system_prompt:

  Optional system prompt (applied to all requests). Prompt caching is
  enabled automatically, reducing costs when the same system prompt is
  shared across many requests.

- output_dir:

  Directory for parquet chunks ("auto" generates timestamped dir)

- overwrite:

  If `FALSE` (default), errors when `output_dir` already contains chunk
  (`.parquet`) or `metadata.json` files. Set to `TRUE` to delete them
  and write fresh outputs; other files are left untouched.

- schema:

  Optional JSON schema for structured output

- concurrent_requests:

  Number of concurrent requests

- temperature:

  Sampling temperature (0-1), included in the request only when
  non-NULL. Dropped with a warning on models that reject sampling
  parameters (Claude Opus 4.7+, Sonnet 5, Fable 5).

- max_tokens:

  Maximum tokens per response

- max_retries:

  Maximum retry attempts per request

- timeout:

  Request timeout in seconds

- key_name:

  Environment variable name for API key

- endpoint_url:

  Anthropic API endpoint URL

- id_col_name:

  Name for ID column in output

- effort:

  Optional reasoning effort, one of "low", "medium", "high", "xhigh",
  "max". Supported on Claude Opus 4.5+, Sonnet 4.6+ and Fable 5; not
  supported on Haiku models.

## Value

A tibble with all results

## Details

This function is designed for processing large text datasets. It divides
input into chunks, processes each chunk with concurrent API requests,
and writes results to disk to minimise memory usage and possibility of
data loss.

Results are written as parquet files in the specified output directory,
along with a metadata.json file containing processing parameters.

When using a custom `output_dir`, existing chunk files are protected by
default. Set `overwrite = TRUE` to replace them.
