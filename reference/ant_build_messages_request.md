# Build an Anthropic Messages API request

Constructs an httr2 request object for Anthropic's Messages API. Handles
message formatting, system prompts, and optional JSON schema for
structured outputs.

## Usage

``` r
ant_build_messages_request(
  input,
  endpointr_id = NULL,
  model = .ANT_DEFAULT_MODEL,
  temperature = 0,
  max_tokens = 4096L,
  schema = NULL,
  system_prompt = NULL,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_MESSAGES_ENDPOINT,
  timeout = 120L,
  max_retries = 5L,
  effort = NULL,
  thinking = NULL
)
```

## Arguments

- input:

  Text input to send to the model

- endpointr_id:

  An id that will persist through to response

- model:

  Anthropic model to use (default: "claude-haiku-5-5")

- temperature:

  Sampling temperature (0-1), included in the request only when
  non-NULL. Dropped with a warning on models that reject sampling
  parameters (Claude Opus 4.7+, Haiku 5.5, Sonnet 5, Fable 5), and when
  `thinking` is "adaptive" or a budget.

- max_tokens:

  Maximum tokens in the response, including thinking tokens

- schema:

  Optional JSON schema for structured output (json_schema object or
  list)

- system_prompt:

  Optional system prompt. When provided, prompt caching is automatically
  enabled via `cache_control`, reducing costs for repeated requests
  sharing the same system prompt.

- key_name:

  Environment variable name for API key

- endpoint_url:

  Anthropic API endpoint URL

- timeout:

  Request timeout in seconds

- max_retries:

  Maximum number of retry attempts for failed requests

- effort:

  Optional effort level, one of "low", "medium", "high", "xhigh", "max".
  Sent as `output_config$effort`. Not accepted by Haiku 4.5 or Sonnet
  4.5. See the "Effort and thinking" section.

- thinking:

  Optional thinking setting. `NULL` (the default) leaves it out of the
  request, so the model uses its own default. One of "adaptive",
  "disabled" or "between_tools", or a whole number for a fixed thinking
  budget in tokens. See the "Effort and thinking" section.

## Value

An httr2 request object

## Details

This function creates the HTTP request but does not execute it.

The `schema` parameter accepts either:

- A `json_schema` S7 object created with
  [`create_json_schema()`](https://jpcompartir.github.io/EndpointR/reference/create_json_schema.md)

- A raw list in Anthropic's `output_config` structure

Anthropic uses `output_config` with a nested `format` field for
structured outputs. This is now GA and requires no beta header.

## Effort and thinking

Most users only need `effort`. It controls how much the model thinks and
how many tokens it uses overall. Lower effort is cheaper and faster. The
default is "high" on most models, and "medium" on Claude Opus 5.5 and
Claude Haiku 5.5. Haiku 4.5 and Sonnet 4.5 don't accept `effort`.

Use `thinking` for two things: to turn thinking on for older models, and
to turn it off on the models that allow it.

- Haiku 4.5, Sonnet 4.5 and Opus 4.5 only think with a fixed budget,
  e.g. `thinking = 2048`. The budget must be at least 1024 and less than
  `max_tokens`.

- Models from 4.6 onwards use `thinking = "adaptive"`, and models from
  4.7 onwards reject a fixed budget.

- `thinking = "disabled"` turns thinking off on Opus 4.7, Opus 4.8,
  Sonnet 5, Opus 5 and Haiku 5.5. Opus 5 and Haiku 5.5 only accept it at
  effort "high" or lower.

- Sonnet 5.5 uses `thinking = "between_tools"` instead, at effort "high"
  or lower.

- Opus 5.5, Fable and Mythos can't turn thinking off. Lower `effort`
  instead.

EndpointR checks these rules before sending, and stops with a message
that suggests a setting that works. Models it doesn't recognise are sent
unchecked.

Thinking tokens count towards `max_tokens`. If a model spends all of
`max_tokens` thinking, the response has no text, `content` is `NA`, and
`stop_reason` is "max_tokens". Raise `max_tokens`, or lower `effort` or
the thinking budget, if that happens. A thinking budget is a target
rather than a hard limit. In our tests, Haiku 4.5 with `thinking = 2048`
sometimes thought for over 4000 tokens, so leave plenty of room above
the budget.

Anthropic reject a changed `temperature` while thinking is on, so when
`thinking` is "adaptive" or a budget, `temperature` is dropped with a
warning.

Before you submit a large batch, send one request with
[`ant_complete_text()`](https://jpcompartir.github.io/EndpointR/reference/ant_complete_text.md)
using the same settings. The API only reports a bad setting in a batch
after the whole batch has run.

## See also

<https://platform.claude.com/docs/en/build-with-claude/structured-outputs>,
<https://platform.claude.com/docs/en/build-with-claude/effort>

## Examples

``` r
if (FALSE) { # \dontrun{
  # simple request
  req <- ant_build_messages_request(
    input = "What is the capital of France?",
    max_tokens = 100
  )

  # less thinking on Haiku 5.5
  req <- ant_build_messages_request(
    input = "What is the capital of France?",
    effort = "low"
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
    max_tokens = 100
  )
} # }
```
