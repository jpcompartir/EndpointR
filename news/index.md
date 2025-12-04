# Changelog

## EndpointR 0.2

- error message and status propagation improvement. Now writes .error,
  .error_msg (standardised across package), and .status. Main change is
  preventing httr2 eating the errors before we can deal with them
- adds parquet writing to oai_complete_df and oai_embed_df
- adds chunks func to oai_embed, and re-writes all batch -\> chunk logic
- implements the Anthropic messages API with structured outputs (via
  BETA)

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
