# Generate a completion for a single text using Anthropic's Messages API

High-level function to generate a completion for a single text string.
Handles request creation, execution, and response processing with
optional structured output support.

## Usage

``` r
ant_complete_text(
  text,
  model = .ANT_DEFAULT_MODEL,
  system_prompt = NULL,
  schema = NULL,
  temperature = 0,
  max_tokens = 4096L,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_MESSAGES_ENDPOINT,
  max_retries = 5L,
  timeout = 120L,
  tidy = TRUE,
  effort = NULL,
  thinking = NULL
)
```

## Arguments

- text:

  Character string to send to the model

- model:

  Anthropic model to use (default: "claude-haiku-5-5")

- system_prompt:

  Optional system prompt

- schema:

  Optional JSON schema for structured output

- temperature:

  Sampling temperature (0-1), included in the request only when
  non-NULL. Dropped with a warning on models that reject sampling
  parameters (Claude Opus 4.7+, Haiku 5.5, Sonnet 5, Fable 5), and when
  `thinking` is "adaptive" or a budget.

- max_tokens:

  Maximum tokens in the response, including thinking tokens

- key_name:

  Environment variable name for API key

- endpoint_url:

  Anthropic API endpoint URL

- max_retries:

  Maximum retry attempts

- timeout:

  Request timeout in seconds

- tidy:

  Whether to parse structured output (default: TRUE)

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

Character string with the model's response, or parsed JSON if schema
provided. `NA` when the model ran out of tokens before writing an
answer, with a warning.

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
`ant_complete_text()` using the same settings. The API only reports a
bad setting in a batch after the whole batch has run.

## Examples

``` r
if (FALSE) { # \dontrun{
  # simple completion
  response <- ant_complete_text(
    text = "Explain quantum computing in simple terms",
    max_tokens = 500
  )

  # test your settings on one text before running a large batch
  ant_complete_text(
    text = "I love this product!",
    model = "claude-sonnet-5-5",
    effort = "low",
    thinking = "between_tools"
  )

  # with structured output
  sentiment_schema <- create_json_schema(
    name = "sentiment",
    schema = schema_object(
      sentiment = schema_enum(c("positive", "negative", "neutral")),
      confidence = schema_number(minimum = 0, maximum = 1),
      required = c("sentiment", "confidence")
    )
  )
  result <- ant_complete_text(
    text = "I love this product!",
    schema = sentiment_schema
  )
} # }
```
