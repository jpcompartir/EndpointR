# Process embedding API response into a tidy format

Converts the nested list response from a Hugging Face Inference API
embedding request into a tidy tibble.

## Usage

``` r
tidy_embedding_response(response)
```

## Arguments

- response:

  An httr2 response object or the parsed JSON response

## Value

A tibble containing the embedding vectors

## Examples

``` r
if (FALSE) { # \dontrun{
  # Process response from httr2 request
  req <- hf_build_request(text, endpoint_url, api_key)
  resp <- httr2::req_perform(req)
  embeddings <- tidy_embedding_response(resp)

  # Process already parsed JSON
  resp_json <- httr2::resp_body_json(resp)
  embeddings <- tidy_embedding_response(resp_json)
} # }
```
