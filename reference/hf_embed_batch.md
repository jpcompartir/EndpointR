# Generate batches of embeddings for a list of texts

High-level function to generate embeddings for multiple text strings.
This function handles batching and parallel processing of embedding
requests, and attempts to handle errors gracefully.

## Usage

``` r
hf_embed_batch(
  texts,
  endpoint_url,
  key_name,
  ...,
  tidy_func = tidy_embedding_response,
  parameters = list(),
  batch_size = 8,
  include_texts = TRUE,
  concurrent_requests = 5,
  max_retries = 5,
  timeout = 10,
  validate = FALSE,
  relocate_col = 2
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

  ellipsis sent to `hf_perform_request` TODO (reserved ATM)

- tidy_func:

  Function to process/tidy the raw API response (default:
  tidy_embedding_response)

- parameters:

  Advanced usage: parameters to pass to the API endpoint.

- batch_size:

  Number of texts to process in one batch

- include_texts:

  Whether to return the original texts in the return tibble

- concurrent_requests:

  Number of requests to send simultaneously

- max_retries:

  Maximum number of retry attempts for failed requests

- timeout:

  Request timeout in seconds

- validate:

  Whether to validate the endpoint before creating the request

- relocate_col:

  Which position in the data frame to relocate the results to.

## Value

A tibble containing the embedding vectors

## Examples

``` r
if (FALSE) { # \dontrun{
  # Generate embeddings for multiple texts using default batch size
  embeddings <- hf_embed_batch(
    texts = c("First example", "Second example", "Third example"),
    endpoint_url = "https://my-endpoint.huggingface.cloud"
  )

  # With custom batch size and concurrent requests
  embeddings <- hf_embed_batch(
    texts = c("First example", "Second example", "Third example"),
    endpoint_url = "https://my-endpoint.huggingface.cloud",
    batch_size = 10,
    concurrent_requests = 5
  )
} # }
```
