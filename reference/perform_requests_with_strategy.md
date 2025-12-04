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

List of httr2_response objects (check status with resp_status()) or
error objects for network failures

## Details

Returns responses in the order that requests were sent. Since requests
use req_error(is_error = ~ FALSE), HTTP error responses (status \>= 400)
are returned as httr2_response objects rather than being thrown as
errors. Callers should check response status with httr2::resp_status()
or use httr2::resps_successes() / httr2::resps_failures() to categorise
responses.

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
