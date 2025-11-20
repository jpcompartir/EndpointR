# Process OpenAI embedding API response into a tidy format

Converts the nested list response from an OpenAI embedding API request
into a tidy tibble with embedding vectors as columns.

## Usage

``` r
tidy_oai_embedding(response)
```

## Arguments

- response:

  An httr2 response object or the parsed JSON response from OpenAI's
  embedding API

## Value

A tibble containing the embedding vectors as columns (V1, V2, etc.) and
optionally an `oai_index` column if present in the response

## Details

This function handles both single document and batch embedding
responses. It extracts the embedding vectors and converts them into a
wide format tibble where each column (V1, V2, ..., Vn) represents one
dimension of the embedding vector. If the response includes index
information, it adds an `oai_index` column to preserve the ordering.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Process response from httr2 request
  req <- oai_build_embedding_request("Hello world")
  resp <- httr2::req_perform(req)
  embeddings <- tidy_oai_embedding(resp)

  # Process already parsed JSON
  resp_json <- httr2::resp_body_json(resp)
  embeddings <- tidy_oai_embedding(resp_json)
} # }
```
