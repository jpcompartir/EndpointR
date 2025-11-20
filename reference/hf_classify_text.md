# Classify text using a Hugging Face Inference API endpoint

Sends text to a Hugging Face classification endpoint and returns the
classification scores. By default, returns a tidied data frame with one
row and columns for each classification label.

## Usage

``` r
hf_classify_text(
  text,
  endpoint_url,
  key_name,
  ...,
  parameters = list(return_all_scores = TRUE),
  tidy = TRUE,
  max_retries = 5,
  timeout = 20,
  validate = FALSE
)
```

## Arguments

- text:

  Character string to classify

- endpoint_url:

  The URL of the Hugging Face Inference API endpoint

- key_name:

  Name of the environment variable containing the API key

- ...:

  Additional arguments passed to `hf_perform_request` and ultimately to
  [`httr2::req_perform`](https://httr2.r-lib.org/reference/req_perform.html)

- parameters:

  Advanced usage: parameters to pass to the API endpoint, defaults to
  `list(return_all_scores = TRUE)`.

- tidy:

  Logical; if TRUE (default), returns a tidied data frame

- max_retries:

  Maximum number of retry attempts for failed requests

- timeout:

  Request timeout in seconds

- validate:

  Logical; whether to validate the endpoint before creating the request

## Value

A tidied data frame with classification scores (if `tidy=TRUE`) or the
raw API response

## Details

This function handles the entire process of creating a request to a
Hugging Face Inference API endpoint for text classification, sending the
request, and processing the response.

The function will automatically retry failed requests according to the
`max_retries` parameter. If `tidy=TRUE` (the default), it transforms the
nested JSON response into a tidy data frame with one row and columns for
each classification label.

If tidying fails, the function returns the raw response with an
informative message.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Basic classification with default parameters
  result <- hf_classify_text(
    text = "This product is excellent!",
    endpoint_url = "redacted",
    key_name = "API_KEY"
  )

  # Classification with custom parameters for a spam detection model
  spam_result <- hf_classify_text(
    text = "URGENT: You've won a free holiday! Call now to claim.",
    endpoint_url = "redacted",
    parameters = list(return_all_scores = TRUE)
  )

  # Get raw response without tidying
  raw_result <- hf_classify_text(
    text = "I love this movie",
    endpoint_url = "redacted",
    key_name = "API_KEY",
    tidy = FALSE
  )
} # }
```
