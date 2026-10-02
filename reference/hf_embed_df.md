# Generate embeddings for texts in a data frame

High-level function to generate embeddings for texts in a data frame.
This function handles the entire process from request creation to
response processing, with options for batching & parallel execution.

Avoid risk of data loss by setting a low-ish chunk_size (e.g. 5,000,
10,000). Each chunk is written to a `.parquet` file in the `output_dir=`
directory, which also contains a `metadata.json` file which tracks
important information such as the endpoint URL used. Be sure to check
any output directories into .gitignore!

## Usage

``` r
hf_embed_df(
  df,
  text_var,
  id_var,
  endpoint_url,
  key_name,
  output_dir = "auto",
  overwrite = FALSE,
  chunk_size = 5000L,
  batch_size = 32L,
  concurrent_requests = 16L,
  max_retries = 5L,
  timeout = 120L,
  progress = TRUE,
  engine = getOption("EndpointR.hf_engine", "auto")
)
```

## Arguments

- df:

  A data frame containing texts to embed

- text_var:

  Name of the column containing text to embed

- id_var:

  Name of the column to use as ID

- endpoint_url:

  The URL of the Hugging Face Inference API endpoint

- key_name:

  Name of the environment variable containing the API key

- output_dir:

  Path to directory for the .parquet chunks

- overwrite:

  If `FALSE` (default), errors when `output_dir` already contains chunk
  (`.parquet`) or `metadata.json` files. Set to `TRUE` to delete them
  and write fresh outputs; other files are left untouched.

- chunk_size:

  The size of each chunk that will be processed and then written to a
  file.

- batch_size:

  Number of texts to send in each request (default: 32)

- concurrent_requests:

  Number of requests to send at once (default: 16)

- max_retries:

  Maximum re-sends for requests that get 429 or 5xx.

- timeout:

  Request timeout in seconds

- progress:

  Whether to display a progress bar

- engine:

  The endpoint's inference engine: `"auto"` (default), `"tei"` or
  `"toolkit"`. See
  [`hf_embed_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_text.md).

## Value

A data frame with the original data plus embedding columns

## Details

See
[`hf_embed_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_chunks.md)
for how texts are batched, retried and split.

## Examples

``` r
if (FALSE) { # \dontrun{
  df <- data.frame(
    id = 1:3,
    text = c("First example", "Second example", "Third example")
  )

  embeddings_df <- hf_embed_df(
    df = df,
    text_var = text,
    id_var = id,
    endpoint_url = "https://my-endpoint.huggingface.cloud",
    key_name = "HF_API_KEY",
    batch_size = 32,
    concurrent_requests = 16
  )
} # }
```
