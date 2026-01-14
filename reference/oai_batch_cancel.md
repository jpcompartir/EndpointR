# Cancel a Running Batch Job on the OpenAI Batch API

Cancels an in-progress batch job. The batch will stop processing new
requests, but requests already being processed may still complete.

## Usage

``` r
oai_batch_cancel(batch_id, key_name = "OPENAI_API_KEY")
```

## Arguments

- batch_id:

  Batch identifier (starts with 'batch\_'), returned by
  oai_batch_start()

- key_name:

  Name of the environment variable containing your API key

## Value

Metadata about the cancelled batch job

## Examples

``` r
if (FALSE) { # \dontrun{
# Cancel a batch job that's taking too long
cancelled <- oai_batch_cancel("batch_abc123")
cancelled$status # Will be "cancelling" or "cancelled"
} # }
```
