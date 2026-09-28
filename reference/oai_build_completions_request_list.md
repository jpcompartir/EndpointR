# Build OpenAI requests for batch processing

Build OpenAI requests for batch processing

## Usage

``` r
oai_build_completions_request_list(
  inputs,
  endpointr_ids = NULL,
  model = .OAI_DEFAULT_MODEL,
  temperature = NULL,
  max_tokens = 500L,
  schema = NULL,
  system_prompt = NULL,
  max_retries = 5L,
  timeout = 30,
  key_name = "OPENAI_API_KEY",
  endpoint_url = "https://api.openai.com/v1/chat/completions"
)
```

## Arguments

- inputs:

  Character vector of text inputs

- endpointr_ids:

  A vector of IDs which will persist through to responses

- model:

  OpenAI model to use

- temperature:

  Sampling temperature (0-2), included in the request only when
  non-NULL. The default NULL omits it, which reasoning models (the GPT-5
  family, o-series) require - they only accept the default temperature.

- max_tokens:

  Maximum tokens per response, sent as OpenAI's `max_completion_tokens`
  (`max_tokens` is deprecated and rejected by reasoning models)

- schema:

  Optional JSON schema for structured output

- system_prompt:

  Optional system prompt

- max_retries:

  Integer; maximum retry attempts (default: 5)

- timeout:

  Numeric; request timeout in seconds (default: 30)

- key_name:

  Environment variable name for API key

- endpoint_url:

  OpenAI API endpoint URL

## Value

List of httr2 request objects
