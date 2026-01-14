# List Batch Jobs on the OpenAI Batch API

Retrieve a paginated list of batch jobs associated with your API key.

## Usage

``` r
oai_batch_list(limit = 20L, after = NULL, key_name = "OPENAI_API_KEY")
```

## Arguments

- limit:

  Maximum number of batch jobs to return

- after:

  Cursor for pagination; batch ID to start after

- key_name:

  Name of the environment variable containing your API key

## Value

A list containing batch job metadata and pagination information

## Examples

``` r
if (FALSE) { # \dontrun{
# List recent batch jobs
batches <- oai_batch_list(limit = 10)

# Paginate through results
next_page <- oai_batch_list(after = batches$last_id)
} # }
```
