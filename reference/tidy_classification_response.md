# Convert Hugging Face classification response to tidy format

Transforms the nested JSON response from a Hugging Face classification
endpoint into a tidy data frame with one row and columns for each
classification label.

## Usage

``` r
tidy_classification_response(response)
```

## Arguments

- response:

  Either an httr2_response object from a Hugging Face API request or a
  parsed JSON object containing classification results

## Value

A data frame with one row and columns for each classification label

## Details

This function expects a specific structure in the response, with each
classification result containing a 'label' and 'score' field. It
flattens the nested structure and pivots the data to create a
wide-format data frame.

The function accepts either a raw `httr2_response` object or a parsed
JSON structure, making it flexible for different workflow patterns.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Process response directly from API call
  response <- hf_perform_request(req)
  tidy_results <- tidy_classification_response(response)

  # Or with an already-parsed JSON object
  json_data <- httr2::resp_body_json(response)
  tidy_results <- tidy_classification_response(json_data)

  # Example of expected output structure
  # A tibble: 1 × 2
  #   positive negative
  #      <dbl>    <dbl>
  # 1    0.982    0.018
} # }
```
