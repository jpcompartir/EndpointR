# Check the Status of a Batch Job on the OpenAI Batch API

Check the Status of a Batch Job on the OpenAI Batch API

## Usage

``` r
oai_batch_status(batch_id, key_name = "OPENAI_API_KEY")
```

## Arguments

- batch_id:

  Batch identifier (starts with 'batch\_'), returned by
  oai_batch_start()

- key_name:

  Name of the environment variable containing your API key

## Value

Metadata about an OpenAI Batch API Job, including status, error_file_id,
output_file_id, input_file_id etc.

## Examples

``` r
if (FALSE) { # \dontrun{
status <- oai_batch_status("batch_abc123")
status$status # e.g., "completed", "in_progress", "failed"
status$output_file_id # File ID for results when completed
} # }
```
