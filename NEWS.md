# EndpointR 0.1.1

-   `oai_complete_chunks()` function to better support for chunking/batching in `oai_complete_df()`
-   `oai_complete_df()` now writes to a file to mitigate the chance of completely lost data

# EndpointR 0.1.0

Initial BETA release, ships with:

-   Support for embeddings and classification with Hugging Face Inference API & Dedicated Inference Endpoints
-   Support for text completion using OpenAI models via the Chat Completions API
-   Support for embeddings with the OpenAI Embeddings API
-   Structured outputs via JSON schemas and validators
