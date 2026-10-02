# Prepare a batch request for multiple texts

Creates an httr2 request object for obtaining a response from a Hugging
Face Inference endpoint for multiple text inputs in a single batch. The
request body depends on the endpoint's inference engine and the task.

## Usage

``` r
hf_build_request_batch(
  inputs,
  parameters = list(),
  endpoint_url,
  key_name,
  max_retries = 5,
  timeout = 120,
  validate = FALSE,
  engine = c("toolkit", "tei"),
  task = c("embed", "classify"),
  max_length = 512L
)
```

## Arguments

- inputs:

  Vector or list of character strings to process in a batch

- parameters:

  Parameters to send with inputs. These override the defaults.

- endpoint_url:

  The URL of the Hugging Face Inference API endpoint

- key_name:

  Name of the environment variable containing the API key

- max_retries:

  Maximum number of attempts for requests that get 429 or 5xx

- timeout:

  Request timeout in seconds

- validate:

  Whether to validate the endpoint before creating the request

- engine:

  `"toolkit"` (default) or `"tei"`

- task:

  `"embed"` (default) or `"classify"`

- max_length:

  Maximum tokens per text for toolkit classification

## Value

An httr2 request object configured for batch processing

## Details

- TEI classification sends each text as a one-element list
  (`[[text], [text]]`), because TEI reads a flat list of 2 texts as one
  sentence pair. It also asks for raw scores, and EndpointR applies the
  softmax (see
  [`tidy_tei_classification_response()`](https://jpcompartir.github.io/EndpointR/reference/tidy_tei_classification_response.md)).

- TEI embeddings send `truncate = true` at the top level of the body.

- On TEI, `parameters` are added to the top level of the body, because
  TEI ignores a `parameters` field.

- Toolkit classification sends `return_all_scores`, `truncation`,
  `max_length` and `batch_size` in `parameters`. Without `batch_size`,
  the toolkit runs one text at a time on the GPU.

Inputs are always sent as a JSON array, even for a single text.

## Examples

``` r
if (FALSE) { # \dontrun{
  batch_req <- hf_build_request_batch(
    inputs = c("First text to embed", "Second text to embed"),
    endpoint_url = "https://my-endpoint.huggingface.cloud/embedding_api",
    key_name = "HF_API_KEY",
    engine = "tei",
    task = "embed"
  )
} # }
```
