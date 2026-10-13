# Process a data frame through Anthropic's Messages API

Takes a data frame with text inputs and processes each row through
Anthropic's Messages API using chunked processing. Results are written
progressively to parquet files and returned as a tibble. Supports
structured outputs via the schema = argument.

## Usage

``` r
ant_complete_df(
  df,
  text_var,
  id_var,
  model = .ANT_DEFAULT_MODEL,
  output_dir = "auto",
  overwrite = FALSE,
  system_prompt = NULL,
  schema = NULL,
  chunk_size = 5000L,
  concurrent_requests = 5L,
  max_retries = 5L,
  timeout = 120L,
  temperature = 0,
  max_tokens = 4096L,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_MESSAGES_ENDPOINT,
  effort = NULL,
  thinking = NULL
)
```

## Arguments

- df:

  Data frame containing text to process

- text_var:

  Column name (unquoted) containing text inputs

- id_var:

  Column name (unquoted) for unique row identifiers

- model:

  Anthropic model to use (default: "claude-haiku-5-5")

- output_dir:

  Directory for parquet chunks

- overwrite:

  If `FALSE` (default), errors when `output_dir` already contains chunk
  (`.parquet`) or `metadata.json` files. Set to `TRUE` to delete them
  and write fresh outputs; other files are left untouched.

- system_prompt:

  Optional system prompt

- schema:

  Optional JSON schema for structured output

- chunk_size:

  Number of texts per chunk

- concurrent_requests:

  Number of concurrent requests

- max_retries:

  Maximum retry attempts

- timeout:

  Request timeout in seconds

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

A tibble with columns: `id_var`, `content`, `.error`, `.error_msg`,
`stop_reason`, `input_tokens`, `output_tokens`, `thinking_tokens`,
`.status` and `.chunk`

## Details

writes results to disk to minimise memory usage and possibility of data
loss.

Results are written as parquet files in the specified output directory,
along with a metadata.json file containing processing parameters.

When using a custom `output_dir`, existing chunk files are protected by
default. Set `overwrite = TRUE` to replace them.

The results have the same `stop_reason`, `input_tokens`, `output_tokens`
and `thinking_tokens` columns as
[`ant_batch_results()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_results.md).
`thinking_tokens` is `NA` when the API doesn't report it.

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
