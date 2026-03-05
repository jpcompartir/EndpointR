# Synchronous vs Asynchronous/Batch APIs

``` r
library(EndpointR)
```

## Introduction

Most of EndpointR’s integrations are with synchronous APIs such as
[Completions](https://platform.openai.com/docs/api-reference/completions)
by OpenAI, Hugging Face’s [Inference
Endpoints](https://huggingface.co/docs/inference-endpoints/en/index),
and Messages by
[Anthropic](https://platform.claude.com/docs/en/api/messages). When
using these APIs, we send a HTTP request, wait a second or two and
receive a response.

However, data scientists often need to process an entire data frame,
resulting in thousands or millions of HTTP requests. This is inefficient
because:

1.  Cost - Providers don’t offer discounts for these requests
2.  Session Blocking - Our coding consoles get blocked for hours at a
    time
3.  Rate Limits - Providers enforce stricter rate limits on these APIs

A solution to these problems is to use providers’ ‘Batch APIs’ which
offer asynchronous results. These often come with a 50% discount and
higher rate limits, with a guarantee of results within a time frame,
e.g. 24 hours.

> **TIP**: It’s worth noting that the results are often ready much
> faster, consider checking in 1-2 hours after triggering the batch.

## OpenAI Batch API

The OpenAI Batch API workflow follows three stages: **prepare**,
**submit**, and **retrieve**. Below are complete examples for embeddings
and completions.

### Batch Embeddings

``` r
# 1. Prepare your data
df <- data.frame(
 id = c("doc_1", "doc_2", "doc_3"),
 text = c(
   "The quick brown fox jumps over the lazy dog",
   "Machine learning is transforming data science",
   "R is a powerful language for statistical computing"
 )
)

# 2. Prepare requests for the Batch API
jsonl_content <- oai_batch_prepare_embeddings(
 df,
 text_var = text,
 id_var = id,
 model = "text-embedding-3-small",
 dimensions = 256
)

# 3. Upload to the Files API
file_info <- oai_batch_file_upload(jsonl_content)
file_info$id
#> "file-abc123..."

# 4. Trigger the batch job
batch_job <- oai_batch_start(
 file_id = file_info$id,
 endpoint = "/v1/embeddings"
)
batch_job$id
#> "batch-xyz789..."

# 5. Check status (repeat until completed)
status <- oai_batch_status(batch_job$id)
status$status
#> "in_progress" ... later ... "completed"

# 6. Download and parse results
content <- oai_file_content(status$output_file_id)
embeddings_df <- oai_batch_parse_embeddings(content)

# Result: tidy data frame with id and embedding dimensions (V1, V2, ..., V256)
embeddings_df
#> # A tibble
#>   custom_id .error .error_msg    V1      V2      V3 ...
#>   <chr>     <lgl>  <chr>      <dbl>   <dbl>   <dbl> ...
#> 1 doc_1     FALSE  NA         0.023  -0.041   0.018 ...
#> 2 doc_2     FALSE  NA        -0.015   0.032   0.044 ...
#> 3 doc_3     FALSE  NA         0.008  -0.027   0.031 ...
```

### Batch Completions

``` r
# 1. Prepare your data
df <- data.frame(
 id = c("q1", "q2", "q3"),
 prompt = c(
   "What is the capital of France?",
   "Explain photosynthesis in one sentence.",
   "What is 2 + 2?"
 )
)

# 2. Prepare requests
jsonl_content <- oai_batch_prepare_completions(
 df,
 text_var = prompt,
 id_var = id,
 model = "gpt-4o-mini",
 system_prompt = "You are a helpful assistant. Be concise.",
 temperature = 0,
 max_tokens = 100
)

# 3. Upload and trigger batch job
file_info <- oai_batch_file_upload(jsonl_content)
batch_job <- oai_batch_start(
 file_id = file_info$id,
 endpoint = "/v1/chat/completions"
)

# 4. Check status and retrieve results
status <- oai_batch_status(batch_job$id)
# ... wait for status$status == "completed" ...

content <- oai_file_content(status$output_file_id)
completions_df <- oai_batch_parse_completions(content)

completions_df
#> # A tibble
#>   custom_id content                                      .error .error_msg
#>   <chr>     <chr>                                        <lgl>  <chr>
#> 1 q1        The capital of France is Paris.              FALSE  NA
#> 2 q2        Photosynthesis converts sunlight into energy FALSE  NA
#> 3 q3        2 + 2 equals 4.                              FALSE  NA
```

### Batch Completions with Structured Output

For classification tasks or when you need structured data back, combine
the Batch API with JSON schemas:

``` r
# 1. Define a schema for sentiment classification
sentiment_schema <- create_json_schema(
 name = "sentiment_analysis",
 schema_object(
   sentiment = schema_enum(
     c("positive", "negative", "neutral"),
     description = "The sentiment of the text"
   ),
   confidence = schema_number(
     description = "Confidence score between 0 and 1"
   )
 )
)

# 2. Prepare data
df <- data.frame(
 id = c("review_1", "review_2", "review_3"),
 text = c(
   "This product is absolutely fantastic! Best purchase ever.",
   "Terrible quality, broke after one day. Complete waste of money.",
   "It's okay, nothing special but does the job."
 )
)

# 3. Prepare requests with schema
jsonl_content <- oai_batch_prepare_completions(
  df,
  text_var = text,
  id_var = id,
  model = "gpt-4o-mini",
  system_prompt = "Analyse the sentiment of the following text.",
  schema = sentiment_schema,
  temperature = 0
)

# 4. Upload and trigger batch job
file_info <- oai_batch_file_upload(jsonl_content)
batch_job <- oai_batch_start(
 file_id = file_info$id,
 endpoint = "/v1/chat/completions"
)

# 5. Retrieve and parse results
status <- oai_batch_status(batch_job$id)
content <- oai_file_content(status$output_file_id)
results_df <- oai_batch_parse_completions(content)

# The content column contains JSON that can be parsed
results_df$content
#> [1] "{\"sentiment\":\"positive\",\"confidence\":0.95}"
#> [2] "{\"sentiment\":\"negative\",\"confidence\":0.92}"
#> [3] "{\"sentiment\":\"neutral\",\"confidence\":0.78}"

# Parse the JSON content into columns
results_df |>
 dplyr::mutate(
   parsed = purrr::map(content, jsonlite::fromJSON)
 ) |>
 tidyr::unnest_wider(parsed)
#> # A tibble
#>   custom_id sentiment confidence .error .error_msg
#>   <chr>     <chr>          <dbl> <lgl>  <chr>
#> 1 review_1  positive        0.95 FALSE  NA
#> 2 review_2  negative        0.92 FALSE  NA
#> 3 review_3  neutral         0.78 FALSE  NA
```

> **Limits**: Each batch file can contain up to 50,000 requests or
> 200MB, whichever is reached first. For larger datasets, split into
> multiple batches.

## Anthropic Message Batches API

Anthropic offers a [Message Batches
API](https://docs.anthropic.com/en/docs/build-with-claude/batch-processing)
with 50% cost savings, up to 100,000 requests per batch, and a 24-hour
completion window. Unlike OpenAI’s file-upload workflow, Anthropic’s API
accepts requests directly — no file upload step is needed.

The workflow is three compositional steps: **create**, **check status**,
and **retrieve results**.

### Batch Messages

``` r
# 1. Create a batch
batch <- ant_batch_create(
  texts = c(
    "What is the capital of France?",
    "Explain photosynthesis in one sentence.",
    "What is 2 + 2?"
  ),
  custom_ids = c("q1", "q2", "q3"),
  system_prompt = "You are a helpful assistant. Be concise.",
  model = "claude-haiku-4-5",
  max_tokens = 100
)
batch$id
#> "msgbatch_abc123..."

# 2. Check status (repeat until processing_status == "ended")
status <- ant_batch_status(batch$id)
status$processing_status
#> "in_progress" ... later ... "ended"

# 3. Retrieve results
results <- ant_batch_results(batch$id)

results
#> # A tibble
#>   custom_id content                                      .error .error_msg stop_reason input_tokens output_tokens
#>   <chr>     <chr>                                        <lgl>  <chr>      <chr>              <int>         <int>
#> 1 q1        The capital of France is Paris.               FALSE  NA        end_turn              15            10
#> 2 q2        Photosynthesis converts sunlight into energy  FALSE  NA        end_turn              18            12
#> 3 q3        2 + 2 equals 4.                               FALSE  NA        end_turn              12             8
```

### Batch Messages with Structured Output

Structured outputs work with the Anthropic Batches API too. Pass a
`json_schema` object to the `schema` parameter:

``` r
# 1. Define a schema
sentiment_schema <- create_json_schema(
  name = "sentiment_analysis",
  schema_object(
    sentiment = schema_enum(
      c("positive", "negative", "neutral"),
      description = "The sentiment of the text"
    ),
    confidence = schema_number(
      description = "Confidence score between 0 and 1"
    )
  )
)

# 2. Create batch with schema
batch <- ant_batch_create(
  texts = c(
    "This product is absolutely fantastic! Best purchase ever.",
    "Terrible quality, broke after one day.",
    "It's okay, nothing special but does the job."
  ),
  custom_ids = c("review_1", "review_2", "review_3"),
  system_prompt = "Analyse the sentiment of the following text.",
  schema = sentiment_schema,
  model = "claude-sonnet-4-5"
)

batch_id <- batch$id
# 3. Check status and retrieve when ended
batch_status <- ant_batch_status(batch_id)$processing_status
```

In Progress Response

      {
        "id": "XXX",
        "type": "message_batch",
        "processing_status": "in_progress",
        "request_counts": {
          "processing": 3,
          "succeeded": 0,
          "errored": 0,
          "canceled": 0,
          "expired": 0
        },
        "ended_at": null,
        "created_at": "2026-00-00T00:00:00.000000+00:00",
        "expires_at": "2026-00-00T00:00:00.000000+00:00",
        "archived_at": null,
        "cancel_initiated_at": null,
        "results_url": null
      }

Complete Repsonse

      {
        "id": "xxx",
        "type": "message_batch",
        "processing_status": "ended",
        "request_counts": {
          "processing": 0,
          "succeeded": 3,
          "errored": 0,
          "canceled": 0,
          "expired": 0
      },
      "ended_at": "1970-01-01T00:00:00.000000+00:00",
      "created_at": "1970-01-01T00:00:00.000000+00:00",
      "expires_at": "1970-01-01T00:00:00.000000+00:00",
      "archived_at": null,
      "cancel_initiated_at": null,
      "results_url": "https://api.anthropic.com/v1/messages/batches/xxx/results"
      }

``` r
if(batch_status == "ended") results <- ant_batch_results(batch_id) # shows 'in_progress' if not completed

# Parse JSON content into columns
results |>
  dplyr::mutate(
    parsed = purrr::map(content, jsonlite::fromJSON)
  ) |>
  tidyr::unnest_wider(parsed)
```

    # A tibble: 3 × 9
      custom_id content                         .error .error_msg stop_reason input_tokens output_tokens sentiment confidence
      <chr>     <chr>                           <lgl>  <chr>      <chr>              <int>         <int> <chr>          <dbl>
    1 review_1  "{\"sentiment\": \"positive\",… FALSE  NA         end_turn             223            17 positive        0.95
    2 review_2  "{\"sentiment\":\"negative\",\… FALSE  NA         end_turn             222            14 negative        0.95
    3 review_3  "{\"sentiment\": \"neutral\", … FALSE  NA         end_turn             224            17 neutral         0.75
    >

### Prompt Caching

When you provide a `system_prompt`,
[`ant_batch_create()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_create.md)
automatically enables [prompt
caching](https://docs.anthropic.com/en/docs/build-with-claude/prompt-caching)
by adding `cache_control` to each request. This means Anthropic will
cache the system prompt and reuse it across requests in the batch,
reducing costs further.

### Batch Management

EndpointR provides functions for managing your batches:

``` r
# List recent batches
ant_batch_list(limit = 10)

# Cancel a batch that's no longer needed
ant_batch_cancel("msgbatch_abc123")
```

### Large-scale Batches (100k+ requests)

The Anthropic Message Batches API supports up to 100,000 requests per
batch. For larger workloads, split your data into chunks and submit
multiple batches. Here’s an example classifying 500,000 documents with
structured outputs:

``` r
# 1. Define your schema
sentiment_schema <- create_json_schema(
  name = "sentiment_analysis",
  schema_object(
    sentiment = schema_enum(
      c("positive", "negative", "neutral"),
      description = "The sentiment of the text"
    ),
    confidence = schema_number(
      description = "Confidence score between 0 and 1"
    )
  )
)

# 2. Your data (500k texts and unique IDs)
texts <- your_500k_texts
ids   <- your_500k_ids

# 3. Split into chunks of up to 100k
chunks <- split(seq_along(texts), ceiling(seq_along(texts) / 100000))

# 4. Submit a batch per chunk
batch_ids <- purrr::map_chr(chunks, function(idx) {
  batch <- ant_batch_create(
    texts = texts[idx],
    custom_ids = ids[idx],
    system_prompt = "Classify the sentiment of the following text.",
    schema = sentiment_schema,
    model = "claude-haiku-4-5"
  )
  batch$id
})
#> 5 batches submitted

# 5. Check statuses (repeat until all are "ended")
purrr::walk(batch_ids, \(id) {
  status <- ant_batch_status(id)
  cli::cli_alert_info("{id}: {status$processing_status}")
})

# 6. Retrieve and combine all results
all_results <- purrr::map(batch_ids, ant_batch_results) |>
  purrr::list_rbind()

# 7. Parse structured JSON content into columns
all_results |>
  dplyr::mutate(parsed = purrr::map(content, jsonlite::fromJSON)) |>
  tidyr::unnest_wider(parsed)
#> # A tibble: 500,000 x 7
#>   custom_id sentiment  confidence .error .error_msg stop_reason input_tokens
#>   <chr>     <chr>           <dbl> <lgl>  <chr>      <chr>              <int>
#> 1 doc_1     positive         0.95 FALSE  NA         end_turn              22
#> 2 doc_2     negative         0.88 FALSE  NA         end_turn              19
#> ...
```

> **TIP**: Prompt caching kicks in automatically when you provide a
> `system_prompt`, so the repeated system prompt across all 500k
> requests is cached — saving both latency and cost on top of the 50%
> batch discount.

> **Limits**: Each batch supports up to 100,000 requests, and results
> are guaranteed within 24 hours. No file upload or cleanup is required
> — the API handles storage internally.

## When to choose Synchronous vs Asynchronous

> For more detail, see [OpenAI’s Batch
> guide](https://platform.openai.com/docs/guides/batch) and [Anthropic’s
> Batch Processing
> guide](https://docs.anthropic.com/en/docs/build-with-claude/batch-processing).

|              | Synchronous                                                                   | Asynchronous (Batch)                                                                   |
|--------------|-------------------------------------------------------------------------------|----------------------------------------------------------------------------------------|
| Cost         | Full price per token                                                          | ~50% Discount per token                                                                |
| Latency      | Real-time                                                                     | Up to 24 hours                                                                         |
| Use Case     | Experimentation, Prompt testing, Schema development, User-facing applications | Recurrent workflows (evals etc.), embedding large datasets, classifying large datasets |
| Data Size    | Up to ~10,000                                                                 | ~10,000+                                                                               |
| Max Requests | Rate-limited                                                                  | OpenAI: 50k per batch, Anthropic: 100k per batch                                       |

> **Recommendation**: Use the Synchronous API when you need immediate
> feedback e.g. prompt or schema development, and for small datasets
> where cost savings are irrelevant. Once everything is figured out,
> move to the Batch API to save on cost.

## Cleaning Up (OpenAI)

Once an OpenAI batch job has been completed, the associated files will
live on the OpenAI API, inside the Files API. Your OpenAI account will
be charged for storage, so it’s best to download the results and save in
your org’s own cloud storage. Anthropic handles storage internally, so
no cleanup is needed for Anthropic batches.

``` r
oai_file_delete(file_info$id) # delete the input file 

oai_file_delete(status$output_file_id) # delete the output file
oai_file_delete(status$error_file_id) # delete the error file
```

> **NOTE**: At the time of writing, OpenAI save information in both the
> Batch API and the Files API, you need to delete your input, output,
> error files from the *Files API*, you cannot delete from the Batch API

## Technical Details (OpenAI)

### Batch Limits

The OpenAI Batch API enforces specific limits per batch file. If your
data exceeds these, you must split it into multiple batch jobs.

- Max Requests per Batch: 50,000

- Max File Size: 200 MB

  > **Warning**: When using Structured Outputs, the JSON schema is
  > repeated for every single request in the batch file. For complex
  > schemas, you may hit the 200 MB file size limit well before you
  > reach the 50,000 row limit.

### Underlying Request Format

EndpointR handles the JSON formatting for you, but for debugging
purposes, it is helpful to know what the API expects. Each line in the
batch file is a JSON object containing a custom_id and the request body.

``` json
{
    "custom_id": "doc_1",
    "method": "POST",
    "url": "/v1/embeddings",
    "body": {
        "input": "The quick brown fox...",
        "model": "text-embedding-3-small",
        "encoding_format": "float"
    }
}
```
