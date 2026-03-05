# Retrieve Results from a Completed Anthropic Message Batch

Downloads and parses results from a completed message batch. The batch
must have `processing_status` of `"ended"` before results can be
retrieved.

## Usage

``` r
ant_batch_results(
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

A tibble with columns: `custom_id`, `content`, `.error`, `.error_msg`,
`stop_reason`, `input_tokens`, `output_tokens`

## Details

Results are returned as a tibble with one row per request. The function
handles all four Anthropic result types: succeeded, errored, canceled,
and expired.

## Examples

``` r
if (FALSE) { # \dontrun{
  results <- ant_batch_results("msgbatch_abc123")
  results$content # model responses
} # }
```
