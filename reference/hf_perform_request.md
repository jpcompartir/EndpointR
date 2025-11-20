# Execute a single embedding request and process the response

Performs a prepared request and returns the response

## Usage

``` r
hf_perform_request(request, ...)
```

## Arguments

- request:

  An httr2 request object created by hf_build_request

- ...:

  ellipsis is sent to
  [`httr2::req_perform`](https://httr2.r-lib.org/reference/req_perform.html),
  e.g. for `path` and `verbosity`arguments.

## Value

A httr2 response object

## Examples

``` r
if (FALSE) { # \dontrun{
  # Create and perform request
  req <- hf_build_request(
    input = "This is a sample text to embed",
    endpoint_url = "https://my-endpoint.huggingface.cloud"
  )
  embeddings <- hf_perform_request(req)

} # }
```
