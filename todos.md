# EndpointR Hugging Face Embeddings Implementation Checklist

## Core Functions

-   [x] `hf_embed_request_single()` - Prepare a single text embedding request
-   [ ] `hf_embed_request_df()` - Prepare requests for texts in a data frame
-   [ ] `hf_embed_request_batch()` - Prepare batched requests for multiple texts
-   [ ] `hf_embed_perform_single()` - Execute a single request and process response
-   [ ] `hf_embed_perform_batch()` - Execute multiple requests, with parallel option
-   [x] `hf_embed_text()` - High-level function for embedding single texts
-   [x] `hf_embed_df()` - High-level function for embedding texts in a data frame

## Utility Functions

-   [x] `tidy_embedding_response()` - Process API responses into tidy format
-   [ ] `embedding_cache_key()` - Generate cache keys for requests
-   [x] `safely_perform_request()` - Error handling wrapper for requests
-   [x] `validate_hf_endpoint()` - Validate endpoint before sending requests

## Implementation Order

1.  Start with single text embedding functionality
2.  Add data frame and batch processing capabilities
3.  Implement parallel processing with proper error handling
4.  Add caching for improved efficiency
5.  Create high-level user-friendly functions
6.  Write/update vignette
