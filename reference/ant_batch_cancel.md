# Cancel an Anthropic Message Batch

Cancels an in-progress message batch. Requests already being processed
may still complete.

## Usage

``` r
ant_batch_cancel(
  batch_id,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_BATCHES_ENDPOINT
)
```

## Arguments

- batch_id:

  Character string of the batch ID (returned by
  [`ant_batch_create()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_create.md))

- key_name:

  Environment variable name for API key

- endpoint_url:

  Anthropic Batches API endpoint URL

## Value

A list of batch metadata reflecting the cancellation

## Examples

``` r
if (FALSE) { # \dontrun{
  ant_batch_cancel("msgbatch_abc123")
} # }
```
