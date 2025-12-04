# EndpointR 0.2

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

