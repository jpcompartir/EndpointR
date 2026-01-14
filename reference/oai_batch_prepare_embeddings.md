# Prepare a Data Frame for the OpenAI Batch API - Embeddings

Prepare a Data Frame for the OpenAI Batch API - Embeddings

## Usage

``` r
oai_batch_prepare_embeddings(
  df,
  text_var,
  id_var,
  model = "text-embedding-3-small",
  dimensions = NULL,
  method = "POST",
  encoding_format = "float",
  endpoint = "/v1/embeddings"
)
```

## Arguments

- df:

  A data frame containing text to process

- text_var:

  Name of the column containing input text

- id_var:

  Name of the column to use as row ID

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

A list of JSON requests

## Details

Takes an entire data frame and turns each row into a valid line of JSON
ready for a .jsonl file upload to the OpenAI Files API + Batch API job
trigger.

Each request must have its own ID, as the Batch API makes no guarantees
about the order the results will be returned in.

To reduce the overall size, and the explanatory power of the Embeddings,
you can set dimensions to lower than the default (which vary based on
model).

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = c("doc_1", "doc_2", "doc_3"),
  text = c("Hello world", "Embedding text", "Another document")
)
jsonl_content <- oai_batch_prepare_embeddings(df, text_var = text, id_var = id)
} # }
```
