# Classify multiple texts using Hugging Face Inference Endpoints

Classifies a batch of texts using a Hugging Face classification endpoint
and returns classification scores in a tidy format. Handles batching,
concurrent requests, and error recovery automatically.

## Usage

``` r
hf_classify_batch(
  texts,
  endpoint_url,
  key_name,
  ...,
  tidy_func = tidy_batch_classification_response,
  parameters = list(return_all_scores = TRUE),
  batch_size = 8,
  progress = TRUE,
  concurrent_requests = 5,
  max_retries = 5,
  timeout = 30,
  include_texts = TRUE,
  relocate_col = 2
)
```

## Arguments

- texts:

  Character vector of texts to classify

- endpoint_url:

  URL of the Hugging Face Inference API endpoint

- key_name:

  Name of environment variable containing the API key

- ...:

  Additional arguments passed to request functions

- tidy_func:

  Function to process API responses, defaults to
  `tidy_batch_classification_response`

- parameters:

  List of parameters for the API endpoint, defaults to
  `list(return_all_scores = TRUE)`

- batch_size:

  Integer; number of texts per batch (default: 8)

- progress:

  Logical; whether to show progress bar (default: TRUE)

- concurrent_requests:

  Integer; number of concurrent requests (default: 5)

- max_retries:

  Integer; maximum retry attempts (default: 5)

- timeout:

  Numeric; request timeout in seconds (default: 20)

- include_texts:

  Logical; whether to include original texts in output (default: TRUE)

- relocate_col:

  Integer; column position for text column (default: 2)

## Value

Data frame with classification scores for each text, plus columns for
original text (if `include_texts=TRUE`), error status, and error
messages

## Details

This function processes multiple texts efficiently by splitting them
into batches and optionally sending concurrent requests. It includes
robust error handling and progress reporting for large batches.

The function automatically handles request failures with retries and
includes error information in the output when requests fail. Original
text order is preserved in the results.

The function does not currently handle
`list(return_all_scores = FALSE)`.

## Examples

``` r
if (FALSE) { # \dontrun{
  texts <- c(
    "This product is brilliant!",
    "Terrible quality, waste of money",
    "Average product, nothing special"
  )

  results <- hf_classify_batch(
    texts = texts,
    endpoint_url = "redacted",
    key_name = "API_KEY",
    batch_size = 3
  )
} # }
```
