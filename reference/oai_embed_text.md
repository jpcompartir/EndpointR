# Generate embeddings for a single text using OpenAI

High-level function to generate embeddings for a single text string
using OpenAI's embedding API. This function handles the entire process
from request creation to response processing.

## Usage

``` r
oai_embed_text(
  text,
  model = "text-embedding-3-small",
  dimensions = NULL,
  max_retries = 5,
  timeout = 20,
  endpoint_url = "https://api.openai.com/v1/embeddings",
  key_name = "OPENAI_API_KEY",
  tidy = TRUE
)
```

## Arguments

- text:

  Character string to generate embeddings for (must be non-empty)

- model:

  OpenAI embedding model to use (default: "text-embedding-3-small")

- dimensions:

  Number of embedding dimensions (NULL uses model default)

- max_retries:

  Maximum retry attempts for failed requests (default: 5)

- timeout:

  Request timeout in seconds (default: 20)

- endpoint_url:

  OpenAI API endpoint URL (default: OpenAI's embedding endpoint)

- key_name:

  Name of environment variable containing the API key (default:
  "OPENAI_API_KEY")

- tidy:

  Whether to return a tidy tibble format (default: TRUE)

## Value

If `tidy = TRUE`, returns a tibble with embedding vectors as columns
(V1, V2, etc.). If `tidy = FALSE`, returns the raw httr2 response
object.

## Details

This function is designed for single text inputs. For processing
multiple texts, use
[`oai_embed_batch()`](https://jpcompartir.github.io/EndpointR/reference/oai_embed_batch.md)
which is more efficient for batch operations.

The function automatically handles API authentication, request retries,
and error handling. By default, it returns a tidy tibble with embedding
vectors as columns, but you can get the raw response by setting
`tidy = FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Generate embeddings for a single text
  embeddings <- oai_embed_text("Hello world")

  # Use a different model with custom dimensions
  embeddings <- oai_embed_text(
    text = "Hello world",
    model = "text-embedding-3-large",
    dimensions = 1024
  )

  # Get raw response instead of tidy format
  raw_response <- oai_embed_text(
    text = "Hello world",
    tidy = FALSE
  )
} # }
```
