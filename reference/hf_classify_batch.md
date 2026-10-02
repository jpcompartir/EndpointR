# Classify multiple texts using Hugging Face Inference Endpoints

Classifies a batch of texts using a Hugging Face classification endpoint
and returns classification scores in a tidy format. Handles batching,
concurrent requests, and error recovery automatically.

## Usage

``` r
hf_classify_batch(
  texts,
  endpoint_url,
  key_name,
  ...,
  tidy_func = NULL,
  parameters = list(),
  batch_size = 32,
  progress = TRUE,
  concurrent_requests = 16,
  max_retries = 5,
  timeout = 120,
  include_texts = TRUE,
  relocate_col = 2,
  max_length = 512L,
  max_chars = 2000L,
  tokenizer = NULL,
  engine = getOption("EndpointR.hf_engine", "auto")
)
```

## Arguments

- texts:

  Character vector of texts to classify

- endpoint_url:

  The URL of the Hugging Face Inference API endpoint

- key_name:

  Name of the environment variable containing the API key

- ...:

  Reserved for future use

- tidy_func:

  Function to process API responses. `NULL` (default) picks
  [`tidy_tei_classification_response()`](https://jpcompartir.github.io/EndpointR/reference/tidy_tei_classification_response.md)
  on TEI and `tidy_batch_classification_response()` on the toolkit. A
  custom function receives the response for one batch and must return
  one row per text.

- parameters:

  Advanced usage: parameters to pass to the API endpoint. These override
  the defaults for the engine.

- batch_size:

  Integer; number of texts per request (default: 32)

- progress:

  Logical; whether to show progress bar (default: TRUE)

- concurrent_requests:

  Integer; number of concurrent requests (default: 16)

- max_retries:

  Integer; maximum re-sends for requests that get 429 or 5xx (default:
  5)

- timeout:

  Numeric; request timeout in seconds (default: 120)

- include_texts:

  Logical; whether to include original texts in output (default: TRUE)

- relocate_col:

  Integer; column position for text column (default: 2)

- max_length:

  Maximum number of tokens per text. Longer texts are cut. `NULL` turns
  client-side cutting off on TEI.

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

## Value

Data frame with classification scores for each text, plus columns for
original text (if `include_texts=TRUE`), error status, and error
messages

## Details

Texts are sent in batches of `batch_size`, with `concurrent_requests`
requests in flight. On the default Inference Toolkit, texts are sorted
by length before batching, because the toolkit pads each batch to its
longest text. Results are returned in input order.

When a batch fails with a client error (400, 413, 422 or 424) or a
network error, it is split in half and sent again, down to single texts,
so only the text at fault fails. Requests that get 429 or 5xx are
re-sent unchanged, up to `max_retries` times. Empty and missing texts
are not sent; they are returned as error rows.

On TEI endpoints, EndpointR asks for raw scores and applies the softmax
in R, and cuts long texts before sending (see
[`hf_classify_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_text.md)).

The function does not currently handle
`list(return_all_scores = FALSE)`.

## Examples

``` r
if (FALSE) { # \dontrun{
  texts <- c(
    "This product is brilliant!",
    "Terrible quality, waste of money",
    "Average product, nothing special"
  )

  results <- hf_classify_batch(
    texts = texts,
    endpoint_url = "redacted",
    key_name = "API_KEY",
    batch_size = 32,
    concurrent_requests = 16
  )
} # }
```
