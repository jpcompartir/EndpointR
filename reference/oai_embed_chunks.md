# Embed text chunks through OpenAI's Embeddings API

This function processes large volumes of text through OpenAI's
Embeddings API in configurable chunks, writing results progressively to
parquet files. It handles concurrent requests, automatic retries, while
managing memory efficiently for large-scale processing.

## Usage

``` r
oai_embed_chunks(
  texts,
  ids,
  model = "text-embedding-3-small",
  dimensions = 1536,
  output_dir = "auto",
  chunk_size = 5000L,
  concurrent_requests = 5L,
  max_retries = 5L,
  timeout = 20L,
  endpoint_url = "https://api.openai.com/v1/embeddings",
  key_name = "OPENAI_API_KEY",
  id_col_name = "id"
)
```

## Arguments

- texts:

  Character vector of texts to process

- ids:

  Vector of unique identifiers corresponding to each text (same length
  as texts)

- model:

  OpenAI embedding model to use (default: "text-embedding-3-small")

- dimensions:

  Number of embedding dimensions (default: 1536 for
  text-embedding-3-small)

- output_dir:

  Path to directory for the .parquet chunks. "auto" generates a
  timestamped directory name. If NULL, uses a temporary directory.

- chunk_size:

  Number of texts to process in each chunk before writing to disk
  (default: 5000)

- concurrent_requests:

  Number of concurrent requests (default: 5)

- max_retries:

  Maximum retry attempts per failed request (default: 5)

- timeout:

  Request timeout in seconds (default: 20)

- endpoint_url:

  OpenAI API endpoint URL (default: OpenAI's embedding endpoint)

- key_name:

  Name of environment variable containing the API key (default:
  "OPENAI_API_KEY")

- id_col_name:

  Name for the ID column in output (default: "id"). When called from
  oai_embed_df(), this preserves the original column name.

## Value

A tibble with columns:

- ID column (name specified by `id_col_name`): Original identifier from
  input

- `.error`: Logical indicating if request failed

- `.error_msg`: Error message if failed, NA otherwise

- `.chunk`: Chunk number for tracking

- Embedding columns (V1, V2, etc.)

## Details

This function is designed for processing large text datasets that may
not fit comfortably in memory. It divides the input into chunks,
processes each chunk with concurrent API requests, and writes results
immediately to disk to minimise memory usage.

The function preserves data integrity by matching results to source
texts through the `ids` parameter. Each chunk is processed independently
with results written as parquet files to the output directory.

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
  # basic usage with automatic directory naming
  result <- oai_embed_chunks(
    texts = my_texts,
    ids = my_ids,
    model = "text-embedding-3-small"
  )

  # large-scale processing with custom settings
  result <- oai_embed_chunks(
    texts = my_texts,
    ids = my_ids,
    output_dir = "my_embeddings",
    chunk_size = 10000,
    dimensions = 512,
    concurrent_requests = 10
  )
} # }
```
