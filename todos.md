# EndpointR Hugging Face Embeddings Implementation Checklist

## Core Functions

-   [x] `hf_embed_request_single()` - Prepare a single text embedding request
-   [ ] `hf_embed_request_df()` - Prepare requests for texts in a data frame
-   [ ] `hf_embed_request_batch()` - Prepare batched requests for multiple texts
-   [ ] `hf_embed_perform_single()` - Execute a single request and process response
-   [ ] `hf_embed_perform_prallel()` - Execute multiple requests in parallel
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

# HF Batch & Parallel implementation

-   at first glance it seems more straightforward to implement:
    -   batch the df
    -   deal with the requests *within the batch* in parallel
    -   package the batches back up into the df Because we can easily keep track of the id column and the text. But, httr2 req_perform_parallel should return concurrent requests in order they arrived. So we can actually batch up the requests to reduce the number of requests being sent, which reduces some of the overhead.

From a design POV, with batch and parallel in the same function, there are too many arguments and there are dependencies within arguments, which are difficult to reason about. Plus, there are all the different ways progress can be reported, which makes it not simple (by httr2 for lists of requests, for parallel requests, by cli:: for batches, etc.).

To make things slightly more complex, from the httr2 docs we see that req_perform_parallel doesn't respect max_retries...

> Additionally, it does not respect the max_tries argument to req_retry() because if you have five requests in flight and the first one gets rate limited, it's likely that all the others do too. This also means that the circuit breaker is never triggered.

Given we're sending to the same API, it should respect req_throttle and req_retry:

> The main limitation of req_perform_parallel() is that it assumes applies req_throttle() and req_retry() are across all requests. This means, for example, that if request 1 is throttled, but request 2 is not, req_perform_parallel() will wait for request 1 before performing request 2. This makes it most suitable for performing many parallel requests to the same host, rather than a mix of different hosts. It's probably possible to remove these limitation, but it's enough work that I'm unlikely to do it unless I know that people would fine it useful: so please let me know!

Batching multiple texts within a request is handled quite straightforwardly with: `req_body_json(list(inputs = sentences[31:60]))`

It's just we have to do more work the other end with the response handling - particularly if we're using safely and parallel etc. Is it worth it?
