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
  max_tokens = 1024L,
  temperature = 0,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_BATCHES_ENDPOINT,
  timeout = 60L
)
```

## Arguments

- texts:

  Character vector of texts to send to the model

- custom_ids:

  Character vector of unique identifiers (same length as texts)

- model:

  Anthropic model to use

- system_prompt:

  Optional system prompt (applied to all requests)

- schema:

  Optional JSON schema for structured output (json_schema object or
  list)

- max_tokens:

  Maximum tokens per response

- temperature:

  Sampling temperature (0-1)

- key_name:

  Environment variable name for API key

- endpoint_url:

  Anthropic Batches API endpoint URL

- timeout:

  Request timeout in seconds

## Value

A list of batch metadata including `$id` for tracking

## Details

Each request in the batch is a standalone Messages API call. The API
supports up to 100,000 requests per batch.

When `system_prompt` is provided, prompt caching is automatically
enabled by adding `cache_control` to each request's params. The API
applies the cache breakpoint to the last cacheable block.

For structured outputs, pass a `json_schema` object to `schema`. This
uses the GA `output_config` format (no beta header required).

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
    system_prompt = "Reply in one word"
  )
  # Check status later
  ant_batch_status(batch$id)
} # }
```
