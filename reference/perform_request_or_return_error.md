# Perform request and return response or error object

Performs a request and returns the response. Since req_error(is_error =
~ FALSE) is set in base_request(), httr2 won't throw errors for HTTP
status codes \>= 400. Instead, callers should check the response status
with httr2::resp_status().

## Usage

``` r
perform_request_or_return_error(request)
```

## Arguments

- request:

  An httr2 request object

## Value

An httr2_response object (check status with resp_status()) or an error
condition
