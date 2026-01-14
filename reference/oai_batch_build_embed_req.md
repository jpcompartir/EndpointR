# Create a single OpenAI Batch API - Embedding request

This function prepares a single row of data for the OpenAI Batch/Files
APIs, where each row should be valid JSON. The APIs do not guarantee the
results will be in the same order, so we need to provide an ID with each
request.

## Usage

``` r
oai_batch_build_embed_req(
  input,
  id,
  model = "text-embedding-3-small",
  dimensions = NULL,
  method = "POST",
  encoding_format = "float",
  endpoint = "/v1/embeddings"
)
```

## Arguments

- input:

  Text input to embed

- id:

  A custom, unique row ID

- model:

  The embedding model to use

- dimensions:

  Number of embedding dimensions (NULL uses model default)

- method:

  The HTTP request type, usually 'POST'

- encoding_format:

  Data type of the embedding values

- endpoint:

  The API endpoint path, e.g. /v1/embeddings

## Value

a row of JSON

## Examples

``` r
if (FALSE) { # \dontrun{
text <- "embed_me"
id <- "id_1"
batch_req <- oai_batch_build_embed_req(text, id)
} # }
```
