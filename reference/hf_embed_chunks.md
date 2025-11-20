# Embed text chunks through Hugging Face Inference Embedding Endpoints

This function is capable of processing large volumes of text through
Hugging Face's Inference Embedding Endpoints. Results are written in
chunks to a file, to avoid out of memory issues.

## Usage

``` r
hf_embed_chunks(
  texts,
  ids,
  endpoint_url,
  output_dir = "auto",
  chunk_size = 5000L,
  concurrent_requests = 5L,
  max_retries = 5L,
  timeout = 10L,
  key_name = "HF_API_KEY",
  id_col_name = "id"
)
```

## Arguments

- texts:

  Character vector of texts to process

- ids:

  Vector of unique identifiers corresponding to each text (same length
  as texts)

- endpoint_url:

  Hugging Face Embedding Endpoint

- output_dir:

  Path to directory for the .parquet chunks

- chunk_size:

  Number of texts to process in each chunk before writing to disk
  (default: 5000)

- concurrent_requests:

  Number of concurrent requests (default: 5)

- max_retries:

  Maximum retry attempts per failed request (default: 5)

- timeout:

  Request timeout in seconds (default: 10)

- key_name:

  Name of environment variable containing the API key (default:
  "HF_API_KEY")

- id_col_name:

  Name for the ID column in output (default: "id"). When called from
  hf_embed_df(), this preserves the original column name.

## Value

A tibble with columns:

- ID column (name specified by `id_col_name`): Original identifier from
  input

- `.error`: Logical indicating if request failed

- `.error_msg`: Error message if failed, NA otherwise

- `.chunk`: Chunk number for tracking

- Embedding columns (V1, V2, etc.)

## Details

This function processes texts in chunks, creating individual requests
for each text within a chunk. The chunk size determines how many texts
are processed before writing results to disk. Within each chunk,
requests are sent with the specified level of concurrency.
