# Build an Anthropic Messages API request

Constructs an httr2 request object for Anthropic's Messages API. Handles
message formatting, system prompts, and optional JSON schema for
structured outputs. When using strucutred outputs you must select the
correct model.

## Usage

``` r
ant_build_messages_request(
  input,
  endpointr_id = NULL,
  model = .ANT_DEFAULT_MODEL,
  temperature = 0,
  max_tokens = 500L,
  schema = NULL,
  system_prompt = NULL,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_MESSAGES_ENDPOINT,
  timeout = 30L,
  max_retries = 5L
)
```

## Arguments

- input:

  Text input to send to the model

- endpointr_id:

  An id that will persist through to response

- model:

  Anthropic model to use (default: "claude-haiku-4.5")

- temperature:

  Sampling temperature (0-2), higher values = more randomness

- max_tokens:

  Maximum tokens in response

- schema:

  Optional JSON schema for structured output (json_schema object or
  list)

- system_prompt:

  Optional system prompt

- key_name:

  Environment variable name for API key

- endpoint_url:

  Anthropic API endpoint URL

- timeout:

  Request timeout in seconds

- max_retries:

  Maximum number of retry attempts for failed requests

## Value

An httr2 request object

## Details

This function creates the HTTP request but does not execute it. For
structured outputs, you must use a supported model (Claude Sonnet 4.5 or
Opus 4.1) and the request will automatically include the required beta
header.

The `schema` parameter accepts either:

- A `json_schema` S7 object created with
  [`create_json_schema()`](https://jpcompartir.github.io/EndpointR/reference/create_json_schema.md)

- A raw list in Anthropic's `output_format` structure

Unlike OpenAI, Anthropic uses `output_format` (not `response_format`)
and the schema structure differs slightly.

## See also

<https://platform.claude.com/docs/en/build-with-claude/structured-outputs>

## Examples

``` r
if (FALSE) { # \dontrun{
  # simple request
  req <- ant_build_messages_request(
    input = "What is the capital of France?",
    max_tokens = 100
  )

  # with structured output
  schema <- create_json_schema(
    name = "capital_response",
    schema = schema_object(
      country = schema_string(),
      capital = schema_string(),
      required = c("country", "capital")
    )
  )
  req <- ant_build_messages_request(
    input = "What is the capital of France?",
    schema = schema,
    max_tokens = 100,
    model = "sonnet-4-5"
  )
} # }
```
