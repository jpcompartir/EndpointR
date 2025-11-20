# Build OpenAI embedding API request

Creates an httr2 request object configured for OpenAI's embedding API.
This is a lower-level function that handles request configuration
including authentication, retries, and timeouts.

## Usage

``` r
oai_build_embedding_request(
  input,
  model = "text-embedding-3-small",
  dimensions = NULL,
  max_retries = 5,
  timeout = 20,
  endpoint_url = "https://api.openai.com/v1/embeddings",
  key_name = "OPENAI_API_KEY",
  verbose = FALSE
)
```

## Arguments

- input:

  Character vector of text(s) to embed

- model:

  OpenAI embedding model to use (default: "text-embedding-3-small")

- dimensions:

  Number of embedding dimensions (NULL uses model default)

- max_retries:

  Maximum retry attempts for failed requests (default: 5)

- timeout:

  Request timeout in seconds (default: 20)

- endpoint_url:

  OpenAI API endpoint URL (default: OpenAI's embedding endpoint)

- key_name:

  Name of environment variable containing the API key (default:
  "OPENAI_API_KEY")

- verbose:

  Whether to enable verbose request logging (default: FALSE)

## Value

An httr2 request object configured for the OpenAI embedding API. The
request object includes a `total_chars` attribute containing the total
character count of the input texts.

## Details

This function builds the HTTP request but does not execute it. The
request can then be performed using
[`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
or the package's
[`hf_perform_request()`](https://jpcompartir.github.io/EndpointR/reference/hf_perform_request.md)
function.

Note that OpenAI has limits on input length - individual inputs cannot
exceed the model's token limit (typically 8192 tokens for embedding
models). Empty strings are not allowed as input.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Build a simple request
  req <- oai_build_embedding_request("Hello world")

  # Build request with custom dimensions
  req <- oai_build_embedding_request(
    input = "Hello world",
    dimensions = 512,
    model = "text-embedding-3-large"
  )

  # Perform the request
  response <- httr2::req_perform(req)
} # }
```
