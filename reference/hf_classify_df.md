# Classify a data frame of texts using Hugging Face Inference Endpoints

Classifies texts in a data frame column using a Hugging Face
classification endpoint, writing results to disk in chunks.

## Usage

``` r
hf_classify_df(
  df,
  text_var,
  id_var,
  endpoint_url,
  key_name,
  max_length = 512L,
  output_dir = "auto",
  overwrite = FALSE,
  tidy_func = NULL,
  chunk_size = 5000,
  batch_size = 32L,
  concurrent_requests = 16,
  max_retries = 5,
  timeout = 120,
  max_chars = 2000L,
  tokenizer = NULL,
  engine = getOption("EndpointR.hf_engine", "auto"),
  progress = TRUE
)
```

## Arguments

- df:

  Data frame containing texts to classify

- text_var:

  Column name containing texts to classify (unquoted)

- id_var:

  Column name to use as identifier for joining (unquoted)

- endpoint_url:

  Hugging Face Classification Endpoint

- key_name:

  Name of environment variable containing the API key

- max_length:

  Maximum number of tokens per text. Longer texts are cut. `NULL` turns
  client-side cutting off on TEI.

- output_dir:

  Path to directory for the .parquet chunks

- overwrite:

  If `FALSE` (default), errors when `output_dir` already contains chunk
  (`.parquet`) or `metadata.json` files. Set to `TRUE` to delete them
  and write fresh outputs; other files are left untouched.

- tidy_func:

  Function to process API responses. `NULL` (default) picks
  [`tidy_tei_classification_response()`](https://jpcompartir.github.io/EndpointR/reference/tidy_tei_classification_response.md)
  on TEI and `tidy_batch_classification_response()` on the toolkit. A
  custom function receives the response for one batch and must return
  one row per text.

- chunk_size:

  Number of texts to process in each chunk before writing to disk
  (default: 5000)

- batch_size:

  Integer; number of texts per request (default: 32)

- concurrent_requests:

  Integer; number of concurrent requests (default: 16)

- max_retries:

  Integer; maximum re-sends for requests that get 429 or 5xx (default:
  5)

- timeout:

  Numeric; request timeout in seconds (default: 120)

- max_chars:

  Character limit used on TEI when no tokeniser is available

- tokenizer:

  On TEI: a Hugging Face model id (e.g. `"org/model"`) or a
  [`tok::tokenizer`](https://rdrr.io/pkg/tok/man/tokenizer.html), used
  with the `tok` package to cut texts to `max_length` tokens. Dedicated
  endpoints do not report their model id, so pass it here.

- engine:

  The endpoint's inference engine: `"auto"` (default) detects it with a
  call to the endpoint's `/info` route, `"tei"` for Text Embeddings
  Inference, `"toolkit"` for the default Hugging Face Inference Toolkit.
  Set the default for a session with
  `options(EndpointR.hf_engine = "tei")`.

- progress:

  Logical; whether to show progress bar (default: TRUE)

## Value

A data frame with the ids, texts and classification scores, plus
`.error`, `.error_msg`, `.status` and `.chunk` columns

## Details

This function extracts texts and IDs from the specified columns and
classifies them with
[`hf_classify_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_chunks.md),
which writes each chunk to a `.parquet` file in `output_dir` and returns
all of the chunks combined.

See
[`hf_classify_batch()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_batch.md)
for how texts are batched, retried and split, and
[`hf_classify_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_text.md)
for how texts are cut on TEI endpoints.

The function does not currently handle
`list(return_all_scores = FALSE)`.

## Examples

``` r
if (FALSE) { # \dontrun{
  df <- data.frame(
    id = 1:3,
    review = c("Excellent service", "Poor quality", "Average experience")
  )

  classified_df <- hf_classify_df(
    df = df,
    text_var = review,
    id_var = id,
    endpoint_url = "redacted",
    key_name = "API_KEY",
    batch_size = 32,
    concurrent_requests = 16
  )
} # }
```
