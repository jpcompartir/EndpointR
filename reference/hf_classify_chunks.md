# Efficiently classify vectors of text in chunks

Classifies large batches of text using a Hugging Face classification
endpoint. Processes texts in chunks with concurrent requests, writes
intermediate results to disk as Parquet files, and returns a combined
data frame of all classifications.

## Usage

``` r
hf_classify_chunks(
  texts,
  ids,
  endpoint_url,
  max_length = 512L,
  tidy_func = tidy_classification_response,
  output_dir = "auto",
  chunk_size = 5000L,
  concurrent_requests = 5L,
  max_retries = 5L,
  timeout = 30L,
  key_name = "HF_API_KEY",
  id_col_name = "id",
  text_col_name = "text"
)
```

## Arguments

- texts:

  Character vector of texts to classify

- ids:

  Vector of unique identifiers corresponding to each text (same length
  as texts)

- endpoint_url:

  Hugging Face Classification Endpoint

- max_length:

  The maximum number of tokens in the text variable. Beyond this cut-off
  everything is truncated.

- tidy_func:

  Function to process API responses, defaults to
  `tidy_classification_response`

- output_dir:

  Path to directory for the .parquet chunks

- chunk_size:

  Number of texts to process in each chunk before writing to disk
  (default: 5000)

- concurrent_requests:

  Integer; number of concurrent requests (default: 5)

- max_retries:

  Integer; maximum retry attempts (default: 5)

- timeout:

  Numeric; request timeout in seconds (default: 30)

- key_name:

  Name of environment variable containing the API key

- id_col_name:

  Name for the ID column in output (default: "id"). When called from
  hf_classify_df(), this preserves the original column name.

- text_col_name:

  Name for the text column in output (default: "text"). When called from
  hf_classify_df(), this preserves the original column name.

## Value

A data frame of classified documents with successes and failures

## Details

The function creates a metadata JSON file in `output_dir` containing
processing parameters and timestamps. Each chunk is saved as a separate
Parquet file before being combined into the final result. Use
`output_dir = "auto"` to generate a timestamped directory automatically.

For single text classification, use
[`hf_classify_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_text.md)
instead.

## Examples

``` r
if (FALSE) { # \dontrun{
# basic usage with vectors
texts <- c("I love this", "I hate this", "This is ok")
ids <- c("review_1", "review_2", "review_3")

results <- hf_classify_chunks(
  texts = texts,
  ids = ids,
  endpoint_url = "https://your-endpoint.huggingface.cloud",
  key_name = "HF_API_KEY"
)
} # }
```
