# Classify a data frame of texts using Hugging Face Inference Endpoints

Classifies texts in a data frame column using a Hugging Face
classification endpoint and joins the results back to the original data
frame.

## Usage

``` r
hf_classify_df(
  df,
  text_var,
  id_var,
  endpoint_url,
  key_name,
  max_length = 512L,
  output_dir = "auto",
  tidy_func = tidy_classification_response,
  chunk_size = 5000,
  concurrent_requests = 1,
  max_retries = 5,
  timeout = 60
)
```

## Arguments

- df:

  Data frame containing texts to classify

- text_var:

  Column name containing texts to classify (unquoted)

- id_var:

  Column name to use as identifier for joining (unquoted)

- endpoint_url:

  URL of the Hugging Face Inference API endpoint

- key_name:

  Name of environment variable containing the API key

- max_length:

  The maximum number of tokens in the text variable. Beyond this cut-off
  everything is truncated.

- output_dir:

  Path to directory for the .parquet chunks

- tidy_func:

  Function to process API responses, defaults to
  `tidy_batch_classification_response`

- chunk_size:

  Number of texts to process in each chunk before writing to disk
  (default: 5000)

- concurrent_requests:

  Integer; number of concurrent requests (default: 1)

- max_retries:

  Integer; maximum retry attempts (default: 5)

- timeout:

  Numeric; request timeout in seconds (default: 30)

## Value

Original data frame with additional columns for classification scores,
or classification results table if row counts don't match

## Details

This function extracts texts and IDs from the specified columns,
classifies them in chunks. It writes
[`hf_classify_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_chunks.md),
and then returns all of the chu

The function preserves the original data frame structure and adds new
columns for classification scores. If the number of rows doesn't match
after processing (due to errors), it returns the classification results
separately with a warning.

The function does not currently handle
`list(return_all_scores = FALSE)`.

## Examples

``` r
if (FALSE) { # \dontrun{
  df <- data.frame(
    id = 1:3,
    review = c("Excellent service", "Poor quality", "Average experience")
  )

  classified_df <- hf_classify_df(
    df = df,
    text_var = review,
    id_var = id,
    endpoint_url = "redacted",
    key_name = "API_KEY"
  )
} # }
```
