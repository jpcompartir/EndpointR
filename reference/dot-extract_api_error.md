# Extract error message from an API response

Extracts a meaningful error message from an httr2 response object.
Handles different API response formats (OpenAI, Anthropic, HuggingFace).

## Usage

``` r
.extract_api_error(response, fallback_message = "Unknown error")
```

## Arguments

- response:

  An httr2_response object, error object, or other response type

- fallback_message:

  Message to return if extraction fails

## Value

Character string containing the error message, or NA_character\_ if
response is successful
