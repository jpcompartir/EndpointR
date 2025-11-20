# Prepare a single text embedding request

Creates an httr2 request object for obtaining a response from a Hugging
Face Inference endpoint for a single text input. The function can be
used for multiple tasks, i.e. for embedding an input, or classifying an
input

## Usage

``` r
hf_build_request(
  input,
  endpoint_url,
  key_name,
  endpointr_id = NULL,
  parameters = list(),
  max_retries = 5,
  timeout = 10,
  validate = FALSE
)
```

## Arguments

- input:

  Character string to get a response for

- endpoint_url:

  The URL of the Hugging Face Inference API endpoint

- key_name:

  Name of the environment variable containing the API key

- endpointr_id:

  a unique identifier for EndpointR to keep track of the request

- parameters:

  Advanced usage: parameters to pass to the API endpoint

- max_retries:

  Maximum number of retry attempts for failed requests

- timeout:

  Request timeout in seconds

- validate:

  Whether to validate the endpoint before creating the request

## Value

An httr2 request object

## Details

For developers, this function can form the basis of single requests, or
a if mapped over a list of requests.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Create request using API key from environment
  req <- hf_build_request(
    input = "This is a sample text to embed",
    endpoint_url = "https://my-endpoint.huggingface.cloud/embedding_api",
    key_name = "HF_API_KEY"
  )

  # Using default key name
  req <- hf_build_request(
    input = "This is a sample text to classify",
    endpoint_url = "https://my-endpoint.huggingface.cloud/classification_api"
  )
} # }
```
