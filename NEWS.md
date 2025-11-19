# Endpointr 0.1.2

-   We extend the coverage of writing to files to the `hf_embed_df()`, `hf_classify_df()` functions and move to a chunk_size argument rather than batch_size.

-   [ ] `hf_embed_df()`, `hf_classify_df()` improved to write to files similarly to the upgrades applied in 0.1.1

    -   [ ] .parquet files

-   [ ] `oai_complete_df`, `oai_complete_chunks`, `oai_embed_df` all write to .parquet files

    -   [ ] Move to chunk_size argument

-   The package takes a dependency on arrow over read_csv, this enables faster writing and reading of files once stored

-   max_length added to `hf_classify` and `hf_embed` functions

-   `hf_get_model_max_length()` function introduced to make it easier to set the max_length argument in `hf_*` functions.

# EndpointR 0.1.1

-   `oai_complete_chunks()` function to better support for chunking/batching in `oai_complete_df()`
-   `oai_complete_df()` now writes to a file to mitigate the chance of completely lost data

# EndpointR 0.1.0

Initial BETA release, ships with:

-   Support for embeddings and classification with Hugging Face Inference API & Dedicated Inference Endpoints
-   Support for text completion using OpenAI models via the Chat Completions API
-   Support for embeddings with the OpenAI Embeddings API
-   Structured outputs via JSON schemas and validators

