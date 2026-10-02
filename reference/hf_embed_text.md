# Generate embeddings for a single text

High-level function to generate embeddings for a single text string.
This function handles the entire process from request creation to
response processing.

## Usage

``` r
hf_embed_text(
  text,
  endpoint_url,
  key_name,
  ...,
  parameters = list(),
  tidy = TRUE,
  max_retries = 5,
  timeout = 120,
  validate = FALSE,
  engine = getOption("EndpointR.hf_engine", "auto")
)
```

## Arguments

- text:

  Character string to get embeddings for

- endpoint_url:

  The URL of the Hugging Face Inference API endpoint

- key_name:

  Name of the environment variable containing the API key

- ...:

  ellipsis sent to `hf_perform_request`, which forwards to
  [`httr2::req_perform`](https://httr2.r-lib.org/reference/req_perform.html)

- parameters:

  Advanced usage: parameters to pass to the API endpoint. On TEI
  endpoints these are added to the top level of the request body.

- tidy:

  Whether to attempt to tidy the response or not

- max_retries:

  Maximum number of retry attempts for failed requests

- timeout:

  Request timeout in seconds

- validate:

  Whether to validate the endpoint before creating the request

- engine:

  The endpoint's inference engine: `"auto"` (default) detects it with a
  call to the endpoint's `/info` route, `"tei"` for Text Embeddings
  Inference, `"toolkit"` for the default Hugging Face Inference Toolkit.
  Set the default for a session with
  `options(EndpointR.hf_engine = "tei")`.

## Value

A tibble containing the embedding vectors

## Details

The text is sent as a batch of one, in the request format for the
endpoint's inference engine (see the `engine` argument).

## Examples

``` r
if (FALSE) { # \dontrun{
  # Generate embeddings using API key from environment
  embeddings <- hf_embed_text(
    text = "This is a sample text to embed",
    endpoint_url = "https://my-endpoint.huggingface.cloud",
    key_name = "HF_API_KEY"
  )
} # }
```
