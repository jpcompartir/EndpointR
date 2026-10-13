# Create an Anthropic Message Batch

Submits a batch of message requests to Anthropic's Message Batches API.
Batches are processed asynchronously with 50% cost savings and a 24-hour
completion window.

## Usage

``` r
ant_batch_create(
  texts,
  custom_ids,
  model = .ANT_DEFAULT_MODEL,
  system_prompt = NULL,
  schema = NULL,
  max_tokens = 4096L,
  temperature = 0,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_BATCHES_ENDPOINT,
  timeout = 60L,
  effort = NULL,
  thinking = NULL
)
```

## Arguments

- texts:

  Character vector of texts to send to the model

- custom_ids:

  Character vector of unique identifiers (same length as texts)

- model:

  Anthropic model to use (default: "claude-haiku-5-5")

- system_prompt:

  Optional system prompt (applied to all requests)

- schema:

  Optional JSON schema for structured output (json_schema object or
  list)

- max_tokens:

  Maximum tokens per response, including thinking tokens

- temperature:

  Sampling temperature (0-1), included in the request only when
  non-NULL. Dropped with a warning on models that reject sampling
  parameters (Claude Opus 4.7+, Haiku 5.5, Sonnet 5, Fable 5), and when
  `thinking` is "adaptive" or a budget.

- key_name:

  Environment variable name for API key

- endpoint_url:

  Anthropic Batches API endpoint URL

- timeout:

  Request timeout in seconds

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

A list of batch metadata including `$id` for tracking

## Details

Each request in the batch is a standalone Messages API call. The API
supports up to 100,000 requests per batch.

Each request is built by the same code as
[`ant_build_messages_request()`](https://jpcompartir.github.io/EndpointR/reference/ant_build_messages_request.md),
so a batch request and a Messages request with the same arguments send
the same fields.

When `system_prompt` is provided, prompt caching is automatically
enabled by adding `cache_control` to the system prompt.

For structured outputs, pass a `json_schema` object to `schema`. This
uses the GA `output_config` format (no beta header required).

The API checks each request only after the whole batch has run, so a bad
setting is accepted when you submit the batch and only shows up as an
error in
[`ant_batch_results()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_results.md),
up to 24 hours later. EndpointR checks the `effort` and `thinking` rules
it knows about before submitting, but test one request with
[`ant_complete_text()`](https://jpcompartir.github.io/EndpointR/reference/ant_complete_text.md)
and the same settings before you submit a large batch.

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

[`ant_batch_status()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_status.md),
[`ant_batch_results()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_results.md),
[`ant_batch_list()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_list.md),
[`ant_batch_cancel()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_cancel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  batch <- ant_batch_create(
    texts = c("Hello", "World"),
    custom_ids = c("t1", "t2"),
    system_prompt = "Reply in one word",
    effort = "low"
  )
  # Check status later
  ant_batch_status(batch$id)
} # }
```
