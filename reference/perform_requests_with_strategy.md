# Perform multiple requests with configurable concurrency strategy

Executes a list of HTTP requests either sequentially or in parallel.
Automatically chooses sequential processing when concurrent_requests = 1
or when there's only one request.

## Usage

``` r
perform_requests_with_strategy(
  requests,
  concurrent_requests = 1,
  progress = TRUE
)
```

## Arguments

- requests:

  List of httr2_request objects to perform

- concurrent_requests:

  Integer specifying maximum number of simultaneous requests (default:
  1)

- progress:

  Logical indicating whether to show progress bar (default: TRUE)

## Value

List of httr2_response objects or error objects for failed requests

## Details

returns responses in the order that requests were sent, and returns
errors in a predictable format.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Sequential processing
  responses <- perform_requests_with_strategy(
    requests = my_requests,
    concurrent_requests = 1
  )

  # Parallel processing with 5 concurrent requests
  responses <- perform_requests_with_strategy(
    requests = my_requests,
    concurrent_requests = 5,
    progress = TRUE
  )
} # }
```
