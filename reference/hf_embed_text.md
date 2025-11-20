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
  max_retries = 3,
  timeout = 10,
  validate = FALSE
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

  Advanced usage: parameters to pass to the API endpoint

- tidy:

  Whether to attempt to tidy the response or not

- max_retries:

  Maximum number of retry attempts for failed requests

- timeout:

  Request timeout in seconds

- validate:

  Whether to validate the endpoint before creating the request

## Value

A tibble containing the embedding vectors

## Examples

``` r
if (FALSE) { # \dontrun{
  # Generate embeddings using API key from environment
  embeddings <- hf_embed_text(
    text = "This is a sample text to embed",
    endpoint_url = "https://my-endpoint.huggingface.cloud"
  )

  # With custom API key environment variable name
  embeddings <- hf_embed_text(
    text = "This is a sample text to embed",
    endpoint_url = "https://my-endpoint.huggingface.cloud",
    key_name = "MY_CUSTOM_API_KEY"
  )
} # }
```
