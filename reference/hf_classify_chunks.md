# Efficiently classify vectors of text in chunks

Classifies large batches of text using a Hugging Face classification
endpoint. Processes texts in chunks, sending several texts per request
and several requests at once, writes intermediate results to disk as
Parquet files, and returns a combined data frame of all classifications.

## Usage

``` r
hf_classify_chunks(
  texts,
  ids,
  endpoint_url,
  max_length = 512L,
  tidy_func = NULL,
  output_dir = "auto",
  overwrite = FALSE,
  chunk_size = 5000L,
  batch_size = 32L,
  concurrent_requests = 16L,
  max_retries = 5L,
  timeout = 120L,
  key_name = "HF_API_KEY",
  id_col_name = "id",
  text_col_name = "text",
  max_chars = 2000L,
  tokenizer = NULL,
  engine = getOption("EndpointR.hf_engine", "auto"),
  progress = TRUE
)
```

## Arguments

- texts:

  Character vector of texts to classify

- ids:

  Vector of unique identifiers corresponding to each text (same length
  as texts)

- endpoint_url:

  Hugging Face Classification Endpoint

- max_length:

  Maximum number of tokens per text. Longer texts are cut. `NULL` turns
  client-side cutting off on TEI.

- tidy_func:

  Function to process API responses. `NULL` (default) picks
  [`tidy_tei_classification_response()`](https://jpcompartir.github.io/EndpointR/reference/tidy_tei_classification_response.md)
  on TEI and `tidy_batch_classification_response()` on the toolkit. A
  custom function receives the response for one batch and must return
  one row per text.

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

  Integer; number of texts per request (default: 32)

- concurrent_requests:

  Integer; number of concurrent requests (default: 16)

- max_retries:

  Integer; maximum re-sends for requests that get 429 or 5xx (default:
  5)

- timeout:

  Numeric; request timeout in seconds (default: 120)

- key_name:

  Name of environment variable containing the API key

- id_col_name:

  Name for the ID column in output (default: "id"). When called from
  hf_classify_df(), this preserves the original column name.

- text_col_name:

  Name for the text column in output (default: "text"). When called from
  hf_classify_df(), this preserves the original column name.

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

A data frame of classified documents with successes and failures

## Details

The function creates a metadata JSON file in `output_dir` containing
processing parameters, the endpoint's inference engine and (on TEI) its
limits, how texts were cut, the number of empty texts and the number of
split batches. Each chunk is saved as a separate Parquet file before
being combined into the final result. Use `output_dir = "auto"` to
generate a timestamped directory automatically.

See
[`hf_classify_batch()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_batch.md)
for how texts are batched, retried and split, and
[`hf_classify_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_text.md)
for how texts are cut on TEI endpoints. The output's text column holds
the original texts, not the cut ones.

## Examples

``` r
if (FALSE) { # \dontrun{
texts <- c("I love this", "I hate this", "This is ok")
ids <- c("review_1", "review_2", "review_3")

results <- hf_classify_chunks(
  texts = texts,
  ids = ids,
  endpoint_url = "https://your-endpoint.huggingface.cloud",
  key_name = "HF_API_KEY"
)
} # }
```
