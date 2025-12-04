# Generate embeddings for texts in a data frame using OpenAI

High-level function to generate embeddings for texts in a data frame
using OpenAI's embedding API. This function handles the entire process
from request creation to response processing, with options for chunking
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
  output_dir = "auto",
  chunk_size = 5000L,
  concurrent_requests = 1L,
  max_retries = 5L,
  timeout = 20L,
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

  Number of embedding dimensions (default: 1536)

- key_name:

  Name of environment variable containing the API key

- output_dir:

  Path to directory for the .parquet chunks. "auto" generates a
  timestamped directory name. If NULL, uses a temporary directory.

- chunk_size:

  Number of texts to process in each chunk before writing to disk
  (default: 5000)

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

A tibble with columns:

- ID column (preserves original column name): Original identifier from
  input

- `.error`: Logical indicating if request failed

- `.error_msg`: Error message if failed, NA otherwise

- `.chunk`: Chunk number for tracking

- Embedding columns (V1, V2, etc.)

## Details

This function extracts texts from a specified column, generates
embeddings using
[`oai_embed_chunks()`](https://jpcompartir.github.io/EndpointR/reference/oai_embed_chunks.md),
and returns the results matched to the original IDs.

The chunking approach enables processing of large data frames without
memory constraints. Results are written progressively as parquet files
(either to a specified directory or auto-generated) and then read back
as the return value.

OpenAI's embedding API allows you to specify the number of dimensions
for the output embeddings, which can be useful for reducing memory
usage, storage costs, or matching specific downstream requirements. The
default is model-specific (1536 for text-embedding-3-small). [OpenAI
Embedding
Updates](https://openai.com/index/new-embedding-models-and-api-updates/)

Avoid risk of data loss by setting a low-ish chunk_size (e.g. 5,000,
10,000). Each chunk is written to a `.parquet` file in the `output_dir=`
directory, which also contains a `metadata.json` file. Be sure to add
output directories to .gitignore!

## Examples

``` r
if (FALSE) { # \dontrun{
  df <- data.frame(
    id = 1:3,
    text = c("First example", "Second example", "Third example")
  )

  # Generate embeddings with default settings
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
    dimensions = 512,  # smaller embeddings
    chunk_size = 10000
  )

  # Use with concurrent requests for faster processing
  embeddings_df <- oai_embed_df(
    df = df,
    text_var = text,
    id_var = id,
    model = "text-embedding-3-large",
    concurrent_requests = 5
  )
} # }
```
