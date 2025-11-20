# Validate that a Hugging Face Inference Endpoint is available

Checks if an endpoint URL is valid and accessible with the provided API
key. This function sends a small test request to verify the endpoint
works.

## Usage

``` r
validate_hf_endpoint(endpoint_url, key_name)
```

## Arguments

- endpoint_url:

  The URL of the Hugging Face Inference API endpoint

- key_name:

  Name of the environment variable containing the API key

## Value

logical TRUE if endpoint is valid, otherwise stops with an error

## Examples

``` r
if (FALSE) { # \dontrun{
  # Validate endpoint retrieving API key from environment
  validate_hf_endpoint(
    endpoint_url = "https://my-endpoint.huggingface.cloud",
    key_name = "HF_API_KEY"
  )

  # Using default key name
  validate_hf_endpoint("https://my-endpoint.huggingface.cloud")
} # }
```
