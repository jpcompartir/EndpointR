# Build OpenAI requests for batch processing

Build OpenAI requests for batch processing

## Usage

``` r
oai_build_completions_request_list(
  inputs,
  endpointr_ids = NULL,
  model = "gpt-4.1-nano",
  temperature = 0,
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

  Sampling temperature

- max_tokens:

  Maximum tokens per response

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
