# Prepare a batch request for multiple texts

Creates an httr2 request object for obtaining a response from a Hugging
Face Inference endpoint for multiple text inputs in a single batch. This
function can be used for various tasks, such as embedding or classifying
multiple inputs simultaneously.

## Usage

``` r
hf_build_request_batch(
  inputs,
  parameters = list(),
  endpoint_url,
  key_name,
  max_retries = 5,
  timeout = 10,
  validate = FALSE
)
```

## Arguments

- inputs:

  Vector or list of character strings to process in a batch

- parameters:

  Parameters to send with inputs

- endpoint_url:

  The URL of the Hugging Face Inference API endpoint

- key_name:

  Name of the environment variable containing the API key

- max_retries:

  Maximum number of retry attempts for failed requests

- timeout:

  Request timeout in seconds

- validate:

  Whether to validate the endpoint before creating the request

## Value

An httr2 request object configured for batch processing

## Details

For developers, this function forms the basis of batch requests,
enabling more efficient processing of multiple inputs in a single API
call.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Create batch request using API key from environment
  batch_req <- hf_build_request_batch(
    inputs = c("First text to embed", "Second text to embed"),
    endpoint_url = "https://my-endpoint.huggingface.cloud/embedding_api",
    key_name = "HF_API_KEY"
  )

  # Using custom timeout and retry settings
  batch_req <- hf_build_request_batch(
    inputs = c("Text one", "Text two", "Text three"),
    endpoint_url = "https://my-endpoint.huggingface.cloud/embedding_api",
    key_name = "HF_API_KEY",
    max_retries = 3,
    timeout = 15
  )
} # }
```
