# EndpointR 0.3.0

## Faster Hugging Face inference

The `hf_*` functions now send several texts per request and several requests at once, so runs on Hugging Face dedicated endpoints are much faster. In tests on 1 October 2026, we classified and embedded all 2.19 million messages of a client dataset in 13 and 20 minutes on A100 endpoints, which would have taken about 76 and 68 hours at the old defaults. On 100,000 texts, the spam classifier went from 8 to about 4,100 texts per second, and BGE M3 went from 9 to about 2,100 texts per second. The new `hf_throughput_benchmark` dataset has the full results, and the [Improving Performance vignette](https://jpcompartir.github.io/EndpointR/articles/improving_performance.html) plots them.

## Detecting the inference engine

Our endpoints run one of two inference engines, and the engines need different request bodies:

- Text Embeddings Inference (TEI) runs our embedding models and, since October 2026, the spam classifier.
- The default Hugging Face Inference Toolkit runs the Spanish sentiment classifier.

Every `hf_embed_*` and `hf_classify_*` function has a new `engine` argument. The default, `"auto"`, calls the endpoint's `/info` route once per session, because TEI has that route and the toolkit doesn't. Set `engine = "tei"` or `engine = "toolkit"` to skip the call, e.g. when `/info` is blocked, or set a default for the session with `options(EndpointR.hf_engine = "tei")`.

The detection call retries 429 and 5xx responses for about 2 minutes, because an endpoint that has scaled to zero returns 503 while it starts.

## Classification on TEI endpoints

- Classification requests to TEI send each text as a one-element list (`[["a"], ["b"]]`), because TEI reads a flat list of 2 texts as one sentence pair and returns one result without an error.
- Requests ask TEI for raw scores, and the new `tidy_tei_classification_response()` applies the softmax in R. A NaN score crashes TEI's own softmax and leaves the endpoint in a failed state, so EndpointR avoids it. With raw scores, a NaN score fails the request with a 424 error, and EndpointR reports the text as an error.
- TEI ignores `max_length`, and the spam classifier returns NaN scores for long texts in fp16, so EndpointR now cuts texts before sending them to a TEI classifier. If the `tok` package is installed and you pass the model id in the new `tokenizer` argument, texts are cut to `max_length` tokens. Otherwise texts are cut to `max_chars` characters (default 2,000), and EndpointR prints a message that explains the difference.

## Batches, retries and empty texts

- `hf_embed_chunks()`, `hf_embed_df()`, `hf_classify_chunks()` and `hf_classify_df()` gain a `batch_size` argument. Results are matched to rows by their position in each batch, so results come back in input order.
- When a batch fails with a 400, 413, 422 or 424 error, a network error, or the wrong number of results, EndpointR splits it in half and sends each half again, down to single texts. Only the text at fault fails.
- Requests that get 429, 502, 503 or 504 are sent again unchanged, up to `max_retries` times. The retries now also work when requests run in parallel, which they didn't before.
- Empty, whitespace and `NA` texts are not sent. They are returned as error rows with the message "Empty or missing text, not sent", so the output has one row per input.
- On the toolkit, texts are sorted by length before batching, because the toolkit pads each batch to its longest text, and the requests set `parameters$batch_size` so the toolkit runs the batch on the GPU at once.
- When `batch_size` is above a TEI endpoint's `max_client_batch_size`, EndpointR lowers it with a warning.
- `metadata.json` now records the engine, `batch_size`, the TEI limits from `/info`, how texts were cut, and counts of empty texts and split batches.

## Other changes

- `hf_get_endpoint_info()` returns `NULL` with a message for toolkit endpoints, where it used to error.
- `hf_build_request_batch()` gains `engine`, `task` and `max_length` arguments and builds the body for the given engine.
- `tok` is a new suggested package.

## Breaking changes

- The `_df`, `_chunks` and `_batch` functions send several texts per request by default. The default `concurrent_requests` is now 16 (was 1 or 5), `batch_size` is 32 (was 8, or one text per request), and `timeout` is 120 seconds (was 10 to 60).
- Classification requests to TEI endpoints use the TEI format, with raw scores and the softmax applied in R.
- `tidy_func` in the classify functions now defaults to `NULL`, which picks the right function for the engine. A custom `tidy_func` gets the response for one batch and must return one row per text. `tidy_classification_response()` only works for a single text, so don't pass it to the batch functions.
- The `endpointr_id` request header is no longer used to match results to rows.
- Empty and missing texts are reported as errors without being sent.
- `hf_classify_batch()` and `hf_classify_chunks()` accept a single text.
- `hf_embed_batch()` and `hf_classify_batch()` always return a `.status` column.
- Results from TEI endpoints are not exactly repeatable between runs. TEI runs in fp16 and combines texts from all waiting requests into one batch, so scores and embeddings change slightly from run to run, whatever the client sends. To make a downstream analysis such as a clustering repeatable, save the embeddings or scores once and reuse them.

# EndpointR 0.2.4

## Overwrite protection for chunked outputs

The chunk-writing functions no longer silently overwrite existing outputs. All of `ant_complete_chunks()`/`ant_complete_df()`, `oai_complete_chunks()`/`oai_complete_df()`, `oai_embed_chunks()`/`oai_embed_df()`, `hf_embed_chunks()`/`hf_embed_df()` and `hf_classify_chunks()`/`hf_classify_df()` gain an `overwrite` argument:

- When `overwrite = FALSE` (the default), the functions abort if `output_dir` already contains `.parquet` chunk files or a `metadata.json`, rather than clobbering previous results.
- When `overwrite = TRUE`, the existing chunk and metadata files are deleted before writing, so the directory only ever holds one run's outputs - stale chunks from a previous run can no longer mix with new results. Other files in the directory are left untouched.
- Alternatively, use `output_dir = "auto"` to write to a fresh timestamped directory.

Note: if you previously relied on re-running into the same `output_dir`, you will now need to pass `overwrite = TRUE`.

## Documentation

- New "OpenAI- and Anthropic-Compatible Providers" section in the [Connecting to Major Model Providers vignette](https://jpcompartir.github.io/EndpointR/articles/llm_providers.html) (plus a README pointer), showing how to reach DeepSeek, Gemini, Groq, OpenRouter, Ollama and similar providers by changing `endpoint_url` and `key_name`.

## API currency fixes

Both provider integrations have been brought up to date with mid-2026 API changes:

**Anthropic**

- `temperature` is now included in requests only when non-NULL, and is dropped with a warning on models that reject sampling parameters (Claude Opus 4.7+, Sonnet 5, Fable 5) - previously these models returned a 400 error. The default remains `0` for models that support it, including the default `claude-haiku-4-5`.
- New `effort` argument on `ant_build_messages_request()`, `ant_complete_text()`, `ant_complete_chunks()` and `ant_complete_df()`, sent as `output_config$effort` ("low", "medium", "high", "xhigh" or "max"). Supported on Claude Opus 4.5+, Sonnet 4.6+ and Fable 5; not supported on Haiku models.

**OpenAI**

- Request bodies now send `max_completion_tokens` instead of the deprecated `max_tokens`, which reasoning models (o-series, GPT-5 family) reject. The R-level argument is still called `max_tokens`.
- `temperature` now defaults to `NULL` and is only included in the request when set explicitly - reasoning models only accept the default temperature. If you relied on the previous `temperature = 0` default, pass it explicitly.
- The default completions model is now `gpt-5.4-nano` (was `gpt-4.1-nano`), OpenAI's current cheapest small model.
- The `"assistants"` purpose has been removed from `oai_file_upload()`, `oai_file_list()` and `oai_batch_upload()` - the OpenAI Assistants API shuts down on 2026-08-26.

## Bug fixes

- Fixed a test that wrote a stray file (`Hello!`) into the package's test directory instead of a temporary file.

# EndpointR 0.2.3

- Bug fix with error message handling, previously passing in raw `error_msg` to cli:: functions, which then interpret as glue, so try to handle '{ }' when they appear in the error messages. Fix is to passing "{error_msg}" already string interpolated. Fix added to OpenAI integrations as well as Anthropic Batch Implementation
- Tests added, and request creation for Ant batches now checks against the RegEx Anthropic provide



# EndpointR 0.2.2

## Anthropic Messages API
-   `ant_build_messages_request()` now automatically enables prompt caching when a `system_prompt` is provided, structuring it as a content block with `cache_control`. This benefits `ant_complete_chunks()` and `ant_complete_df()` where many requests share the same system prompt — cached reads cost 90% less than uncached.
- Structured outputs is out of BETA and is now generally available, so the header is removed, and `output_form` --> `output_config` in the body of the request following [Anthropic Docs on Structured Outputs](https://platform.claude.com/docs/en/build-with-claude/structured-outputs)

## Anthropic Batch API

Functions for dealing with Anthropic Bathches API, works differently ot the OpenAI API - as we send requests not files.

- `ant_batch_create()` 
- `ant_batch_status()` 
- `ant_batch_results()`
- `ant_batch_list()`
- `ant_batch_cancel()`
- 

See the [Sync Async Vignette](https://jpcompartir.github.io/EndpointR/articles/sync_async.html#anthropic-message-batches-api) for more details

# EndpointR 0.2.1

## OpenAI Batch API

Adds support for OpenAI's asynchronous Batch API, offering 50% cost savings and higher rate limits compared to synchronous endpoints. Ideal for large-scale embeddings, classifications, and batch inference tasks.

**Request preparation:**

-   `oai_batch_build_embed_req()` - Build a single embedding request row
-   `oai_batch_prepare_embeddings()` - Prepare an entire data frame for batch embeddings
-   `oai_batch_build_completions_req()` - Build a single chat completions request row
-   `oai_batch_prepare_completions()` - Prepare an entire data frame for batch completions (supports structured outputs via JSON schema)

**Job management:**

-   `oai_batch_upload()` - Upload prepared JSONL to OpenAI Files API
-   `oai_batch_start()` - Trigger a batch job on an uploaded file
-   `oai_batch_status()` - Check the status of a running batch job
-   `oai_batch_list()` - List all batch jobs associated with your API key
-   `oai_batch_cancel()` - Cancel an in-progress batch job

**Results parsing:**

-   `oai_batch_parse_embeddings()` - Parse batch embedding results into a tidy data frame
-   `oai_batch_parse_completions()` - Parse batch completion results into a tidy data frame

## OpenAI Files API

-   `oai_file_list()` - List files uploaded to the OpenAI Files API
-   `oai_file_content()` - Retrieve the content of a file (e.g., batch results)
-   `oai_file_delete()` - Delete a file from the Files API

# EndpointR 0.2.0

-   error message and status propagation improvement. Now writes .error, .error_msg (standardised across package), and .status. Main change is preventing httr2 eating the errors before we can deal with them
-   adds parquet writing to oai_complete_df and oai_embed_df
-   adds chunks func to oai_embed, and re-writes all batch -\> chunk logic
-   implements the Anthropic messages API with structured outputs (via BETA)
-   adds `ant_complete_df()` and `ant_complete_chunks()` for batch/chunked processing with the Anthropic API, with parquet writing and metadata tracking
-   metadata tracking now includes `schema` and `system_prompt` for both OpenAI and Anthropic chunked processing functions
-   bug fix: S7 schema objects now correctly serialised to metadata.json (previously caused "No method asJSON S3 class: S7_object" error)
-   adds spelling test, sets language to en-GB in DESCRIPTION

# EndpointR 0.1.2

-   **File writing improvements**: `hf_embed_df()` and `hf_classify_df()` now write intermediate results as `.parquet` files to `output_dir` directories, similar to improvements in 0.1.1 for OpenAI functions

-   **Parameter changes**: Moved from `batch_size` to `chunk_size` argument across `hf_embed_df()`, `hf_classify_df()`, and `oai_complete_df()` for consistency

-   **New chunking functions**: Introduced `hf_embed_chunks()` and `hf_classify_chunks()` for more efficient batch processing with better error handling

-   **Dependency update**: Package now depends on `arrow` for faster `.parquet` file writing and reading

-   **Metadata tracking**: Hugging Face functions that write to files (`hf_embed_df()`, `hf_classify_df()`, `hf_embed_chunks()`, `hf_classify_chunks()`) now write `metadata.json` to output directories containing:

    -   Endpoint URL and API key name used
    -   Processing parameters (chunk_size, concurrent_requests, timeout, max_retries)
    -   Inference parameters (truncate, max_length)
    -   Timestamp and row counts
    -   Useful for debugging, reproducibility, and tracking which models/endpoints were used

-   **max_length parameter**: Added `max_length` parameter to `hf_classify_df()` and `hf_classify_chunks()` for text truncation control. Note: `hf_embed_df()` handles truncation automatically via endpoint configuration (set `AUTO_TRUNCATE` in endpoint settings)

-   **New utility functions**:

    -   `hf_get_model_max_length()` - Retrieve maximum token length for a Hugging Face model
    -   `hf_get_endpoint_info()` - Retrieve detailed information about a Hugging Face Inference Endpoint

-   **Improved reporting**: Chunked/batch processing functions now report total successes and failures at completion

# EndpointR 0.1.1

-   `oai_complete_chunks()` function to better support for chunking/batching in `oai_complete_df()`
-   `oai_complete_df()` now writes to a file to mitigate the chance of completely lost data

# EndpointR 0.1.0

Initial BETA release, ships with:

-   Support for embeddings and classification with Hugging Face Inference API & Dedicated Inference Endpoints
-   Support for text completion using OpenAI models via the Chat Completions API
-   Support for embeddings with the OpenAI Embeddings API
-   Structured outputs via JSON schemas and validators
