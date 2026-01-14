# Create a Single OpenAI Batch API - Chat Completions Request

This function prepares a single row of data for the OpenAI Batch/Files
APIs, where each row should be valid JSON. The APIs do not guarantee the
results will be in the same order, so we need to provide an ID with each
request.

## Usage

``` r
oai_batch_build_completions_req(
  input,
  id,
  model = "gpt-4o-mini",
  system_prompt = NULL,
  temperature = 0,
  max_tokens = 500L,
  schema = NULL,
  method = "POST",
  endpoint = "/v1/chat/completions"
)
```

## Arguments

- input:

  Text input (user message) for the completion

- id:

  A custom, unique row ID

- model:

  The chat completion model to use

- system_prompt:

  Optional system prompt to guide the model's behaviour

- temperature:

  Sampling temperature (0 = deterministic, higher = more random)

- max_tokens:

  Maximum number of tokens to generate

- schema:

  Optional JSON schema for structured output (json_schema object or
  list)

- method:

  The HTTP request type, usually 'POST'

- endpoint:

  The API endpoint path, e.g. /v1/chat/completions

## Value

A row of JSON suitable for the Batch API

## Examples

``` r
if (FALSE) { # \dontrun{
req <- oai_batch_build_completions_req(
  input = "What is the capital of France?",
  id = "query_1",
  model = "gpt-4o-mini",
  temperature = 0
)
} # }
```
