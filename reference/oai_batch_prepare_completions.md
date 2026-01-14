# Prepare a Data Frame for the OpenAI Batch API - Chat Completions

Takes an entire data frame and turns each row into a valid line of JSON
ready for a .jsonl file upload to the OpenAI Files API + Batch API job
trigger.

## Usage

``` r
oai_batch_prepare_completions(
  df,
  text_var,
  id_var,
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

- df:

  A data frame containing text to process

- text_var:

  Name of the column containing input text

- id_var:

  Name of the column to use as row ID

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

A character string of newline-separated JSON requests

## Details

Each request must have its own ID, as the Batch API makes no guarantees
about the order the results will be returned in.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = c("q1", "q2"),
  prompt = c("What is 2+2?", "Explain gravity briefly.")
)
jsonl_content <- oai_batch_prepare_completions(
  df,
  text_var = prompt,
  id_var = id,
  system_prompt = "You are a helpful assistant."
)
} # }
```
