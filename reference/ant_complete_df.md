# Process a data frame through Anthropic's Messages API

Takes a data frame with text inputs and processes each row through
Anthropic's Messages API using chunked processing. Results are written
progressively to parquet files and returned as a tibble. Supports
structured outputs via the schema = argument.

## Usage

``` r
ant_complete_df(
  df,
  text_var,
  id_var,
  model = "claude-haiku-4-5",
  output_dir = "auto",
  system_prompt = NULL,
  schema = NULL,
  chunk_size = 5000L,
  concurrent_requests = 5L,
  max_retries = 5L,
  timeout = 30,
  temperature = 0,
  max_tokens = 1024L,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_MESSAGES_ENDPOINT
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

  Anthropic model to use

- output_dir:

  Directory for parquet chunks

- system_prompt:

  Optional system prompt

- schema:

  Optional JSON schema for structured output

- chunk_size:

  Number of texts per chunk

- concurrent_requests:

  Number of concurrent requests

- max_retries:

  Maximum retry attempts

- timeout:

  Request timeout in seconds

- temperature:

  Sampling temperature

- max_tokens:

  Maximum tokens per response

- key_name:

  Environment variable name for API key

- endpoint_url:

  Anthropic API endpoint URL

## Value

A tibble with results

## Details

writes results to disk to minimise memory usage and possibility of data
loss.

Results are written as parquet files in the specified output directory,
along with a metadata.json file containing processing parameters.

When using the `output_dir =` argument, be careful that you select a new
directory if you do not wish to overwrite existing chunks. If there is
already a `chunks_001.parquet` file in the directory, it will be
overwritten.
