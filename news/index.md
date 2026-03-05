# Changelog

## EndpointR 0.2.2

### Anthropic Messages API

- [`ant_build_messages_request()`](https://jpcompartir.github.io/EndpointR/reference/ant_build_messages_request.md)
  now automatically enables prompt caching when a `system_prompt` is
  provided, structuring it as a content block with `cache_control`. This
  benefits
  [`ant_complete_chunks()`](https://jpcompartir.github.io/EndpointR/reference/ant_complete_chunks.md)
  and
  [`ant_complete_df()`](https://jpcompartir.github.io/EndpointR/reference/ant_complete_df.md)
  where many requests share the same system prompt — cached reads cost
  90% less than uncached.
- Structured outputs is out of BETA and is now generally available, so
  the header is removed, and `output_form` –\> `output_config` in the
  body of the request following [Anthropic Docs on Structured
  Outputs](https://platform.claude.com/docs/en/build-with-claude/structured-outputs)

### Anthropic Batch API

Functions for dealing with Anthropic Bathches API, works differently ot
the OpenAI API - as we send requests not files.

- [`ant_batch_create()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_create.md)
- [`ant_batch_status()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_status.md)
- [`ant_batch_results()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_results.md)
- [`ant_batch_list()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_list.md)
- [`ant_batch_cancel()`](https://jpcompartir.github.io/EndpointR/reference/ant_batch_cancel.md)
- 

See the [Sync Async
Vignette](https://jpcompartir.github.io/EndpointR/news/articles/sync_async.html#anthropic-message-batches-api)
for more details

## EndpointR 0.2.1

### OpenAI Batch API

Adds support for OpenAI’s asynchronous Batch API, offering 50% cost
savings and higher rate limits compared to synchronous endpoints. Ideal
for large-scale embeddings, classifications, and batch inference tasks.

**Request preparation:**

- [`oai_batch_build_embed_req()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_build_embed_req.md) -
  Build a single embedding request row
- [`oai_batch_prepare_embeddings()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_prepare_embeddings.md) -
  Prepare an entire data frame for batch embeddings
- [`oai_batch_build_completions_req()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_build_completions_req.md) -
  Build a single chat completions request row
- [`oai_batch_prepare_completions()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_prepare_completions.md) -
  Prepare an entire data frame for batch completions (supports
  structured outputs via JSON schema)

**Job management:**

- [`oai_batch_upload()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_upload.md) -
  Upload prepared JSONL to OpenAI Files API
- [`oai_batch_start()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_start.md) -
  Trigger a batch job on an uploaded file
- [`oai_batch_status()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_status.md) -
  Check the status of a running batch job
- [`oai_batch_list()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_list.md) -
  List all batch jobs associated with your API key
- [`oai_batch_cancel()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_cancel.md) -
  Cancel an in-progress batch job

**Results parsing:**

- [`oai_batch_parse_embeddings()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_parse_embeddings.md) -
  Parse batch embedding results into a tidy data frame
- [`oai_batch_parse_completions()`](https://jpcompartir.github.io/EndpointR/reference/oai_batch_parse_completions.md) -
  Parse batch completion results into a tidy data frame

### OpenAI Files API

- [`oai_file_list()`](https://jpcompartir.github.io/EndpointR/reference/oai_file_list.md) -
  List files uploaded to the OpenAI Files API
- [`oai_file_content()`](https://jpcompartir.github.io/EndpointR/reference/oai_file_content.md) -
  Retrieve the content of a file (e.g., batch results)
- [`oai_file_delete()`](https://jpcompartir.github.io/EndpointR/reference/oai_file_delete.md) -
  Delete a file from the Files API

## EndpointR 0.2.0

- error message and status propagation improvement. Now writes .error,
  .error_msg (standardised across package), and .status. Main change is
  preventing httr2 eating the errors before we can deal with them
- adds parquet writing to oai_complete_df and oai_embed_df
- adds chunks func to oai_embed, and re-writes all batch -\> chunk logic
- implements the Anthropic messages API with structured outputs (via
  BETA)
- adds
  [`ant_complete_df()`](https://jpcompartir.github.io/EndpointR/reference/ant_complete_df.md)
  and
  [`ant_complete_chunks()`](https://jpcompartir.github.io/EndpointR/reference/ant_complete_chunks.md)
  for batch/chunked processing with the Anthropic API, with parquet
  writing and metadata tracking
- metadata tracking now includes `schema` and `system_prompt` for both
  OpenAI and Anthropic chunked processing functions
- bug fix: S7 schema objects now correctly serialised to metadata.json
  (previously caused “No method asJSON S3 class: S7_object” error)
- adds spelling test, sets language to en-GB in DESCRIPTION

## EndpointR 0.1.2

- **File writing improvements**:
  [`hf_embed_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_df.md)
  and
  [`hf_classify_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_df.md)
  now write intermediate results as `.parquet` files to `output_dir`
  directories, similar to improvements in 0.1.1 for OpenAI functions

- **Parameter changes**: Moved from `batch_size` to `chunk_size`
  argument across
  [`hf_embed_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_df.md),
  [`hf_classify_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_df.md),
  and
  [`oai_complete_df()`](https://jpcompartir.github.io/EndpointR/reference/oai_complete_df.md)
  for consistency

- **New chunking functions**: Introduced
  [`hf_embed_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_chunks.md)
  and
  [`hf_classify_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_chunks.md)
  for more efficient batch processing with better error handling

- **Dependency update**: Package now depends on `arrow` for faster
  `.parquet` file writing and reading

- **Metadata tracking**: Hugging Face functions that write to files
  ([`hf_embed_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_df.md),
  [`hf_classify_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_df.md),
  [`hf_embed_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_chunks.md),
  [`hf_classify_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_chunks.md))
  now write `metadata.json` to output directories containing:

  - Endpoint URL and API key name used
  - Processing parameters (chunk_size, concurrent_requests, timeout,
    max_retries)
  - Inference parameters (truncate, max_length)
  - Timestamp and row counts
  - Useful for debugging, reproducibility, and tracking which
    models/endpoints were used

- **max_length parameter**: Added `max_length` parameter to
  [`hf_classify_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_df.md)
  and
  [`hf_classify_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_chunks.md)
  for text truncation control. Note:
  [`hf_embed_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_df.md)
  handles truncation automatically via endpoint configuration (set
  `AUTO_TRUNCATE` in endpoint settings)

- **New utility functions**:

  - [`hf_get_model_max_length()`](https://jpcompartir.github.io/EndpointR/reference/hf_get_model_max_length.md) -
    Retrieve maximum token length for a Hugging Face model
  - [`hf_get_endpoint_info()`](https://jpcompartir.github.io/EndpointR/reference/hf_get_endpoint_info.md) -
    Retrieve detailed information about a Hugging Face Inference
    Endpoint

- **Improved reporting**: Chunked/batch processing functions now report
  total successes and failures at completion

## EndpointR 0.1.1

- [`oai_complete_chunks()`](https://jpcompartir.github.io/EndpointR/reference/oai_complete_chunks.md)
  function to better support for chunking/batching in
  [`oai_complete_df()`](https://jpcompartir.github.io/EndpointR/reference/oai_complete_df.md)
- [`oai_complete_df()`](https://jpcompartir.github.io/EndpointR/reference/oai_complete_df.md)
  now writes to a file to mitigate the chance of completely lost data

## EndpointR 0.1.0

Initial BETA release, ships with:

- Support for embeddings and classification with Hugging Face Inference
  API & Dedicated Inference Endpoints
- Support for text completion using OpenAI models via the Chat
  Completions API
- Support for embeddings with the OpenAI Embeddings API
- Structured outputs via JSON schemas and validators
