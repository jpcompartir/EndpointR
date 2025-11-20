# Generate embeddings for texts in a data frame

High-level function to generate embeddings for texts in a data frame.
This function handles the entire process from request creation to
response processing, with options for batching & parallel execution.
Setting the number of retries

Avoid risk of data loss by setting a low-ish chunk_size (e.g. 5,000,
10,000). Each chunk is written to a `.parquet` file in the `output_dir=`
directory, which also contains a `metadata.json` file which tracks
important information such as the endpoint URL used. Be sure to check
any output directories into .gitignore!

## Usage

``` r
hf_embed_df(
  df,
  text_var,
  id_var,
  endpoint_url,
  key_name,
  output_dir = "auto",
  chunk_size = 5000L,
  concurrent_requests = 1L,
  max_retries = 5L,
  timeout = 15L,
  progress = TRUE
)
```

## Arguments

- df:

  A data frame containing texts to embed

- text_var:

  Name of the column containing text to embed

- id_var:

  Name of the column to use as ID

- endpoint_url:

  The URL of the Hugging Face Inference API endpoint

- key_name:

  Name of the environment variable containing the API key

- output_dir:

  Path to directory for the .parquet chunks

- chunk_size:

  The size of each chunk that will be processed and then written to a
  file.

- concurrent_requests:

  Number of requests to send at once. Some APIs do not allow for
  multiple requests.

- max_retries:

  Maximum number of retry attempts for failed requests.

- timeout:

  Request timeout in seconds

- progress:

  Whether to display a progress bar

## Value

A data frame with the original data plus embedding columns

## Examples

``` r
if (FALSE) { # \dontrun{
  # Generate embeddings for a data frame
  df <- data.frame(
    id = 1:3,
    text = c("First example", "Second example", "Third example")
  )

  # Use batching without parallel processing
  embeddings_df <- hf_embed_df(
    df = df,
    text_var = text,
    endpoint_url = "https://my-endpoint.huggingface.cloud",
    id_var = id
  )

  # Use both chunking and parallel processing
  embeddings_df <- hf_embed_df(
    df = df,
    text_var = text,
    endpoint_url = "https://my-endpoint.huggingface.cloud",
    id_var = id,
    chunk_size = 10000,
    concurrent_requests = 50
  )
} # }
```
