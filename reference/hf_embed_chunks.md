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
  overwrite = FALSE,
  chunk_size = 5000L,
  batch_size = 32L,
  concurrent_requests = 16L,
  max_retries = 5L,
  timeout = 120L,
  key_name = "HF_API_KEY",
  id_col_name = "id",
  engine = getOption("EndpointR.hf_engine", "auto"),
  progress = TRUE
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

- overwrite:

  If `FALSE` (default), errors when `output_dir` already contains chunk
  (`.parquet`) or `metadata.json` files. Set to `TRUE` to delete them
  and write fresh outputs; other files are left untouched.

- chunk_size:

  Number of texts to process in each chunk before writing to disk
  (default: 5000)

- batch_size:

  Number of texts to send in each request (default: 32)

- concurrent_requests:

  Number of concurrent requests (default: 16)

- max_retries:

  Maximum re-sends for requests that get 429 or 5xx (default: 5)

- timeout:

  Request timeout in seconds (default: 120)

- key_name:

  Name of environment variable containing the API key (default:
  "HF_API_KEY")

- id_col_name:

  Name for the ID column in output (default: "id"). When called from
  hf_embed_df(), this preserves the original column name.

- engine:

  The endpoint's inference engine: `"auto"` (default), `"tei"` or
  `"toolkit"`. See
  [`hf_embed_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_text.md).

- progress:

  Whether to show a progress bar

## Value

A tibble with columns:

- ID column (name specified by `id_col_name`): Original identifier from
  input

- `.error`: Logical indicating if request failed

- `.error_msg`: Error message if failed, NA otherwise

- `.status`: HTTP status code of a failed request, NA otherwise

- `.chunk`: Chunk number for tracking

- Embedding columns (V1, V2, etc.)

## Details

This function processes texts in chunks. Within each chunk, texts are
sent in batches of `batch_size` texts per request, with
`concurrent_requests` requests in flight. After each chunk, its results
are written to a `.parquet` file in `output_dir`.

When a batch fails with a client error (400, 413, 422 or 424) or a
network error, it is split in half and sent again, down to single texts,
so only the text at fault fails. Empty and missing texts are not sent;
they are returned as error rows. Results are returned in input order.

The engine, batch size, endpoint limits (on TEI), number of empty texts
and number of split batches are recorded in `metadata.json`.
