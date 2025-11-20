# Generate embeddings for multiple texts using OpenAI

High-level function to generate embeddings for multiple text strings
using OpenAI's embedding API. This function handles batching, concurrent
requests, error handling, and provides progress reporting for large
collections of texts.

## Usage

``` r
oai_embed_batch(
  texts,
  model = "text-embedding-3-small",
  dimensions = 1536,
  batch_size = 10,
  concurrent_requests = 1,
  max_retries = 5,
  timeout = 20,
  endpoint_url = "https://api.openai.com/v1/embeddings",
  key_name = "OPENAI_API_KEY",
  include_texts = TRUE,
  relocate_col = 2,
  verbose = FALSE
)
```

## Arguments

- texts:

  Vector or list of character strings to generate embeddings for

- model:

  OpenAI embedding model to use (default: "text-embedding-3-small")

- dimensions:

  Number of embedding dimensions (default: 1536 for
  text-embedding-3-small)

- batch_size:

  Number of texts to process in one API request (default: 10)

- concurrent_requests:

  Number of requests to send simultaneously (default: 1)

- max_retries:

  Maximum retry attempts for failed requests (default: 5)

- timeout:

  Request timeout in seconds (default: 20)

- endpoint_url:

  OpenAI API endpoint URL (default: OpenAI's embedding endpoint)

- key_name:

  Name of environment variable containing the API key (default:
  "OPENAI_API_KEY")

- include_texts:

  Whether to include original texts in the result (default: TRUE)

- relocate_col:

  Column position to place error columns (default: 2)

- verbose:

  Whether to enable verbose request logging (default: FALSE)

## Value

A tibble containing:

- Embedding vectors as columns (V1, V2, ..., Vn)

- .error: Logical column indicating if embedding failed

- .error_message: Character column with error details

- text: Original texts (if include_texts = TRUE)

## Details

This function efficiently processes multiple texts by:

1.  Splitting texts into batches of the specified size

2.  Creating concurrent requests (if configured) for faster processing

3.  Handling individual batch failures gracefully

4.  Pre-allocating memory for embeddings to improve performance

5.  Providing detailed success/failure reporting

If a batch fails, only the documents in that specific batch will be
marked as failed, not all documents across all batches. Failed
embeddings will be filled with NA values and marked with error
information.

The function returns a tibble with embedding columns (V1, V2, ..., Vn),
error tracking columns (.error, .error_message), and optionally the
original texts.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Basic batch embedding
  texts <- c("First text", "Second text", "Third text")
  embeddings <- oai_embed_batch(texts)

  # Large-scale processing with concurrent requests
  large_texts <- rep("Sample text", 100)
  embeddings <- oai_embed_batch(
    texts = large_texts,
    batch_size = 20,
    concurrent_requests = 5,
    dimensions = 512
  )

  # Custom model and settings
  embeddings <- oai_embed_batch(
    texts = texts,
    model = "text-embedding-3-large",
    dimensions = 1024,
    include_texts = FALSE,
    timeout = 30
  )
} # }
```
