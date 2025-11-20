# Create a base HTTP POST request for API endpoints

Constructs a base httr2 POST request object with common headers and
authentication. This function sets up the foundation for API requests
with standard configuration.

## Usage

``` r
base_request(endpoint_url, api_key)
```

## Arguments

- endpoint_url:

  Character string containing the API endpoint URL

- api_key:

  Character string containing the API authentication key

## Value

An httr2_request object configured with POST method, JSON content type,
and bearer token authentication

## Examples

``` r
if (FALSE) { # \dontrun{
  # Create a base POST request for an API endpoint
  req <- base_request(
    endpoint_url = "https://api.example.com/v1/endpoint",
    api_key = "your-api-key-here"
  )
} # }
```
