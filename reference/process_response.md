# Process API response with error handling

Higher-order function that applies a tidying function to an API
response. Handles both successful responses and errors, returning a
consistent tibble structure. The `tidy_func` parameter allows you to
provide the necessary function for your particular workflow.

## Usage

``` r
process_response(resp, indices, tidy_func)
```

## Arguments

- resp:

  An httr2_response object or error object from a failed request

- indices:

  Vector of indices to track original position of requests

- tidy_func:

  Function to process/tidy successful API responses

## Value

A tibble with processed results or error information, including:

- original_index: Position in original request batch

- .error: Logical indicating if an error occurred

- .error_msg: Character description of any error

- Additional columns from tidy_func output

## Examples

``` r
if (FALSE) { # \dontrun{
  # Process a response with custom tidying function
  result <- process_response(
    resp = api_response,
    indices = c(1, 2, 3),
    tidy_func = function(r) { tibble::tibble(data = httr2::resp_body_json(r)) }
  )
} # }
```
