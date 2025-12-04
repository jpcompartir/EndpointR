# Safely perform an embedding request with error handling

Wrapper around httr2::req_perform that handles errors gracefully.
Returns the response object directly - check status with
httr2::resp_status().

## Usage

``` r
safely_perform_request(request)
```

## Arguments

- request:

  An httr2 request object

## Value

A list with components \$result (httr2_response or NULL) and \$error
(NULL or condition)
