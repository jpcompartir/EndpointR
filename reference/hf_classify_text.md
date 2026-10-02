# Classify text using a Hugging Face Inference API endpoint

Sends text to a Hugging Face classification endpoint and returns the
classification scores. By default, returns a tidied data frame with one
row and columns for each classification label.

## Usage

``` r
hf_classify_text(
  text,
  endpoint_url,
  key_name,
  ...,
  parameters = list(),
  tidy = TRUE,
  max_retries = 5,
  timeout = 120,
  validate = FALSE,
  max_length = 512L,
  max_chars = 2000L,
  tokenizer = NULL,
  engine = getOption("EndpointR.hf_engine", "auto")
)
```

## Arguments

- text:

  Character string to classify

- endpoint_url:

  The URL of the Hugging Face Inference API endpoint

- key_name:

  Name of the environment variable containing the API key

- ...:

  Additional arguments passed to `hf_perform_request` and ultimately to
  [`httr2::req_perform`](https://httr2.r-lib.org/reference/req_perform.html)

- parameters:

  Advanced usage: parameters to pass to the API endpoint. These override
  the defaults for the engine.

- tidy:

  Logical; if TRUE (default), returns a tidied data frame

- max_retries:

  Maximum number of retry attempts for failed requests

- timeout:

  Request timeout in seconds

- validate:

  Logical; whether to validate the endpoint before creating the request

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

A tidied data frame with classification scores (if `tidy=TRUE`) or the
raw API response

## Details

The text is sent as a batch of one, in the request format for the
endpoint's inference engine (see the `engine` argument).

On the default Inference Toolkit, `max_length` is sent to the endpoint.
TEI ignores it and only cuts texts at the model's own limit, and long
texts can give NaN scores, so on TEI EndpointR cuts the text before
sending it. With the `tok` package and a `tokenizer`, it cuts at exactly
`max_length` tokens; otherwise it cuts at `max_chars` characters.

If tidying fails, the function returns the raw response with an
informative message.

## Examples

``` r
if (FALSE) { # \dontrun{
  result <- hf_classify_text(
    text = "This product is excellent!",
    endpoint_url = "redacted",
    key_name = "API_KEY"
  )

  # Get raw response without tidying
  raw_result <- hf_classify_text(
    text = "I love this movie",
    endpoint_url = "redacted",
    key_name = "API_KEY",
    tidy = FALSE
  )
} # }
```
