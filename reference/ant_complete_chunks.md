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
  schema = NULL,
  concurrent_requests = 5L,
  temperature = 0,
  max_tokens = 1024L,
  max_retries = 5L,
  timeout = 30L,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_MESSAGES_ENDPOINT,
  id_col_name = "id"
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

  Optional system prompt (applied to all requests)

- output_dir:

  Directory for parquet chunks ("auto" generates timestamped dir)

- schema:

  Optional JSON schema for structured output

- concurrent_requests:

  Number of concurrent requests

- temperature:

  Sampling temperature

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

## Value

A tibble with all results

## Details

This function is designed for processing large text datasets. It divides
input into chunks, processes each chunk with concurrent API requests,
and writes results to disk to minimise memory usage and possibility of
data loss.

Results are written as parquet files in the specified output directory,
along with a metadata.json file containing processing parameters.

When using the `output_dir =` argument, be careful that you select a new
directory if you do not wish to overwrite existing chunks. If there is
already a `chunks_001.parquet` file in the directory, it will be
overwritten.
