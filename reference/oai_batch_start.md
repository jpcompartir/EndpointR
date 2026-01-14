# Trigger a batch job to run on an uploaded file

Trigger a batch job to run on an uploaded file

## Usage

``` r
oai_batch_start(
  file_id,
  endpoint = c("/v1/embeddings", "/v1/chat/completions"),
  completion_window = "24h",
  metadata = NULL,
  key_name = "OPENAI_API_KEY"
)
```

## Arguments

- file_id:

  File ID returned by oai_batch_upload()

- endpoint:

  The API endpoint path, e.g. /v1/embeddings

- completion_window:

  Time window for batch completion (OpenAI guarantees 24h only)

- metadata:

  Optional list of metadata to tag the batch with

- key_name:

  Name of the environment variable containing your API key

## Value

Metadata about an OpenAI Batch Job Including the batch ID

## Details

Once a file has been uploaded to the OpenAI Files API it's necessary to
trigger the batch job. This will ensure that your file is processed, and
processing is finalised within the 24 hour guarantee.

It's important to choose the right endpoint. If processing should be
done by the Completions API, be sure to route to v1/chat/completions,
and this must match each row in your uploaded file.

Batch Job Ids start with "batch\_", you'll receive a warning if you try
to check batch status on a Files API file (the Files/Batch API set up is
a lil bit clumsy for me)

## Examples

``` r
if (FALSE) { # \dontrun{
# After uploading a file with oai_batch_upload()
batch_job <- oai_batch_start(
  file_id = "file-abc123",
  endpoint = "/v1/embeddings"
)
batch_job$id # Use this to check status later
} # }
```
