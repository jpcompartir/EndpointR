# Generate batches of embeddings for a list of texts

High-level function to generate embeddings for multiple text strings.
This function sends several texts per request and several requests at
once, and attempts to handle errors gracefully.

## Usage

``` r
hf_embed_batch(
  texts,
  endpoint_url,
  key_name,
  ...,
  tidy_func = tidy_embedding_response,
  parameters = list(),
  batch_size = 32,
  include_texts = TRUE,
  concurrent_requests = 16,
  max_retries = 5,
  timeout = 120,
  validate = FALSE,
  relocate_col = 2,
  engine = getOption("EndpointR.hf_engine", "auto"),
  progress = TRUE
)
```

## Arguments

- texts:

  Vector or list of character strings to get embeddings for

- endpoint_url:

  The URL of the Hugging Face Inference API endpoint

- key_name:

  Name of the environment variable containing the API key

- ...:

  Reserved for future use

- tidy_func:

  Function to process/tidy the raw API response (default:
  tidy_embedding_response)

- parameters:

  Advanced usage: parameters to pass to the API endpoint. On TEI
  endpoints these are added to the top level of the request body.

- batch_size:

  Number of texts to send in each request (default: 32)

- include_texts:

  Whether to return the original texts in the return tibble

- concurrent_requests:

  Number of requests to send simultaneously (default: 16)

- max_retries:

  Maximum number of re-sends for requests that get 429 or 5xx

- timeout:

  Request timeout in seconds

- validate:

  Whether to validate the endpoint before creating the request

- relocate_col:

  Which position in the data frame to relocate the results to.

- engine:

  The endpoint's inference engine: `"auto"` (default), `"tei"` or
  `"toolkit"`. See
  [`hf_embed_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_text.md).

- progress:

  Whether to show a progress bar

## Value

A tibble containing the embedding vectors

## Details

Texts are sent in batches of `batch_size`, with `concurrent_requests`
requests in flight. When a batch fails with a client error (400, 413,
422 or 424) or a network error, it is split in half and sent again, down
to single texts, so only the text at fault fails. Requests that get 429
or 5xx are re-sent unchanged, up to `max_retries` times.

Empty and missing texts are not sent. They are returned as error rows.

TEI endpoints reject requests with more texts than their
`max_client_batch_size` (32 by default). When `batch_size` is larger,
EndpointR lowers it with a warning.

## Examples

``` r
if (FALSE) { # \dontrun{
  embeddings <- hf_embed_batch(
    texts = c("First example", "Second example", "Third example"),
    endpoint_url = "https://my-endpoint.huggingface.cloud",
    key_name = "HF_API_KEY",
    batch_size = 32,
    concurrent_requests = 16
  )
} # }
```
