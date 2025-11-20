# Safely perform an embedding request with error handling

Wrapper around httr2::req_perform that handles errors gracefully.

## Usage

``` r
safely_perform_request(request)
```

## Arguments

- request:

  An httr2 request object

## Value

A list with components \$result and \$error
