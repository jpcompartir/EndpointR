# Build the body of an Anthropic Messages request

Shared by the Messages route and the Batches route (where it becomes
each request's `params`), so the two routes send the same fields. Checks
the arguments, drops `temperature` where the API would reject it, and
stops when `effort` and `thinking` can't work on `model`.

## Usage

``` r
.ant_build_body(
  input,
  model,
  max_tokens,
  temperature,
  system_prompt,
  schema,
  effort,
  thinking,
  call = rlang::caller_env()
)
```

## Arguments

- input:

  Text input to send to the model

- model:

  Anthropic model to use (default: "claude-haiku-5-5")

- max_tokens:

  Maximum tokens in the response, including thinking tokens

- temperature:

  Sampling temperature (0-1), included in the request only when
  non-NULL. Dropped with a warning on models that reject sampling
  parameters (Claude Opus 4.7+, Haiku 5.5, Sonnet 5, Fable 5), and when
  `thinking` is "adaptive" or a budget.

- system_prompt:

  Optional system prompt. When provided, prompt caching is automatically
  enabled via `cache_control`, reducing costs for repeated requests
  sharing the same system prompt.

- schema:

  Optional JSON schema for structured output (json_schema object or
  list)

- effort:

  Optional effort level, one of "low", "medium", "high", "xhigh", "max".
  Sent as `output_config$effort`. Not accepted by Haiku 4.5 or Sonnet
  4.5. See the "Effort and thinking" section.

- thinking:

  Optional thinking setting. `NULL` (the default) leaves it out of the
  request, so the model uses its own default. One of "adaptive",
  "disabled" or "between_tools", or a whole number for a fixed thinking
  budget in tokens. See the "Effort and thinking" section.

- call:

  The user-facing function to name in error messages

## Value

A list, ready to be sent as JSON
