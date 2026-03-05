# List Anthropic Message Batches

Retrieves a paginated list of message batches.

## Usage

``` r
ant_batch_list(
  limit = 20L,
  before_id = NULL,
  after_id = NULL,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_BATCHES_ENDPOINT
)
```

## Arguments

- limit:

  Maximum number of batches to return (1-1000, default 20)

- before_id:

  Cursor for backward pagination (batch ID)

- after_id:

  Cursor for forward pagination (batch ID)

- key_name:

  Environment variable name for API key

- endpoint_url:

  Anthropic Batches API endpoint URL

## Value

A list containing batch metadata and pagination information

## Examples

``` r
if (FALSE) { # \dontrun{
  batches <- ant_batch_list(limit = 10)
} # }
```
