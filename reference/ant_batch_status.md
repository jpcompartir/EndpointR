# Check the Status of an Anthropic Message Batch

Retrieves the current status and metadata for a message batch.

## Usage

``` r
ant_batch_status(
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

A list of batch metadata including `processing_status`,
`request_counts`, etc.

## Examples

``` r
if (FALSE) { # \dontrun{
  status <- ant_batch_status("msgbatch_abc123")
  status$processing_status # e.g. "in_progress", "ended"
} # }
```
