# Generate embeddings for texts in a data frame using OpenAI

High-level function to generate embeddings for texts in a data frame
using OpenAI's embedding API. This function handles the entire process
from request creation to response processing, with options for batching
& concurrent requests.

## Usage

``` r
oai_embed_df(
  df,
  text_var,
  id_var,
  model = "text-embedding-3-small",
  dimensions = 1536,
  key_name = "OPENAI_API_KEY",
  batch_size = 10,
  concurrent_requests = 1,
  max_retries = 5,
  timeout = 20,
  endpoint_url = "https://api.openai.com/v1/embeddings",
  progress = TRUE
)
```

## Arguments

- df:

  Data frame containing texts to embed

- text_var:

  Column name (unquoted) containing texts to embed

- id_var:

  Column name (unquoted) for unique row identifiers

- model:

  OpenAI embedding model to use (default: "text-embedding-3-small")

- dimensions:

  Number of embedding dimensions (NULL uses model default)

- key_name:

  Name of environment variable containing the API key

- batch_size:

  Number of texts to process in one batch (default: 10)

- concurrent_requests:

  Number of concurrent requests (default: 1)

- max_retries:

  Maximum retry attempts per request (default: 5)

- timeout:

  Request timeout in seconds (default: 20)

- endpoint_url:

  OpenAI API endpoint URL

- progress:

  Whether to display a progress bar (default: TRUE)

## Value

Original data frame with additional columns for embeddings (V1, V2,
etc.), plus .error and .error_message columns indicating any failures

## Details

This function extracts texts from a specified column, generates
embeddings using
[`oai_embed_batch()`](https://jpcompartir.github.io/EndpointR/reference/oai_embed_batch.md),
and joins the results back to the original data frame using a specified
ID column.

The function preserves the original data frame structure and adds new
columns for embedding dimensions (V1, V2, ..., Vn). If the number of
rows doesn't match after processing (due to errors), it returns the
results with a warning.

OpenAI's embedding API allows you to specify the number of dimensions
for the output embeddings, which can be useful for reducing memory
usage, storage cost,s or matching specific downstream requirements. The
default is model-specific (1536 for text-embedding-3-small). [OpenAI
Embedding
Updates](https://openai.com/index/new-embedding-models-and-api-updates/)

## Examples

``` r
if (FALSE) { # \dontrun{
  df <- data.frame(
    id = 1:3,
    text = c("First example", "Second example", "Third example")
  )

  # Generate embeddings with default dimensions
  embeddings_df <- oai_embed_df(
    df = df,
    text_var = text,
    id_var = id
  )

  # Generate embeddings with custom dimensions
  embeddings_df <- oai_embed_df(
    df = df,
    text_var = text,
    id_var = id,
    dimensions = 360,  # smaller embeddings
    batch_size = 5
  )

  # Use with concurrent requests for faster processing
  embeddings_df <- oai_embed_df(
    df = df,
    text_var = text,
    id_var = id,
    model = "text-embedding-3-large",
    concurrent_requests = 3
  )
} # }
```
