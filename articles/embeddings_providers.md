# Embeddings Providers

This vignette shows how to generate text embeddings using EndpointR with
both Hugging Face and OpenAI providers.

## What are Text Embeddings?

Text embeddings are numerical representations of text that capture
semantic meaning. Think of them as coordinates in a high-dimensional
space where similar texts are closer together. They’re the foundation
for:

- Semantic search
- Clustering similar documents  
- Finding duplicates
- Building recommendation systems
- Powering RAG (Retrieval-Augmented Generation) applications

EndpointR makes it easy to generate embeddings from your text data using
either Hugging Face or OpenAI APIs.

## Setup

``` r
library(EndpointR)
library(dplyr)
library(tibble)

sample_texts <- tibble(
  id = 1:3,
  text = c(
    "Machine learning is transforming how we process information",
    "I love building applications with embeddings", 
    "Natural language processing enables computers to understand text"
  ),
  category = c("ML", "embeddings", "NLP")
)
```

## Provider Comparison

Before diving into code, let’s understand the key differences:

| Feature                                       | Hugging Face                           | OpenAI                                |
|-----------------------------------------------|----------------------------------------|---------------------------------------|
| **Models**                                    | Many open-source models                | text-embedding-3-small/large, ada-002 |
| **Dimensions**                                | Model-dependent (often 384, 768)       | Configurable (512-3072)               |
| **Pricing**                                   | Free tier available, pay for dedicated | Pay per token                         |
| **Rate Limits**                               | Varies by tier                         | Generous for most use cases           |
| **Max Input**                                 | Model-dependent                        | 8,192 tokens per request              |
| **Batching (multiple documents per request)** | Supported                              | Supported                             |

## Hugging Face Embeddings

### Setting Up

First, get your API key from [Hugging
Face](https://huggingface.co/settings/tokens) and set it. Then set your
endpoint’s URL. Here we’ve chosen an endpoint for accessing embeddings
from the `all-mpnet-base-v2` model.

``` r
set_api_key("HF_TEST_API_KEY")

embed_url <-  "https://router.huggingface.co/hf-inference/models/sentence-transformers/all-mpnet-base-v2/pipeline/feature-extraction"
```

### Single Text

The simplest case - embed one piece of text:

``` r
embedding <- hf_embed_text(
  text = "I want to understand the meaning of this sentence",
  endpoint_url = embed_url,
  key_name = "HF_TEST_API_KEY"
)

dim(embedding) # result: a tibble with 768 columns (V1 to V768)

embedding
```

### Batch Processing

For multiple texts, use
[`hf_embed_batch()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_batch.md)
which handles batching automatically. We feed in a vector of inputs and
a `batch_size`, and the function takes care of batching our vector into
as many batches as necessary.

``` r
texts_to_embed <- c(
  "First document about machine learning",
  "Second document about deep learning",
  "Third document about neural networks",
  "Fourth document about data science"
)

batch_embeddings <- hf_embed_batch(
  texts = texts_to_embed,
  endpoint_url = embed_url,
  key_name = "HF_TEST_API_KEY",
  batch_size = 2,  # process 2 texts per API call
  concurrent_requests = 2  # run 2 requests in parallel
)

# Check results
glimpse(batch_embeddings[1,1:10 ]) # truncated for ease
```

The result includes: - `text`: your original text - `.error` and
`.error_msg`: error tracking - `V1` to `V768`: the embedding dimensions

### Data Frame Integration

Most commonly, you’ll want to embed a column from a data frame:

``` r
embedded_df <- hf_embed_df(
  df = sample_texts,
  text_var = text,      # column containing text
  id_var = id,          # unique identifier column
  endpoint_url = embed_url,
  key_name = "HF_TEST_API_KEY",
  batch_size = 3,
  concurrent_requests = 1
)

# Original data + embeddings
names(embedded_df)[1:10]  # shows: id, text, category, .error, .error_msg, V1, V2...

embedded_df
```

## OpenAI Embeddings

### Setting Up

Get your API key from the [OpenAI](https://platform.openai.com/api-keys)
website and set it:

``` r
set_api_key("OPENAI_API_KEY")
```

### Single Text

OpenAI offers configurable embedding dimensions:

``` r
# Default dimensions (1536 for text-embedding-3-small)
embedding <- oai_embed_text(
  text = "I want to understand the meaning of this sentence"
)

# Custom dimensions for smaller embeddings
small_embedding <- oai_embed_text(
  text = "I want to understand the meaning of this sentence",
  model = "text-embedding-3-small",
  dimensions = 512  # reduce size by ~67%
)

dim(small_embedding)  # 1 row, 512 embedding columns + index
```

### Batch Processing

OpenAI allows multiple texts in a single API call, which
[`oai_embed_batch()`](https://jpcompartir.github.io/EndpointR/reference/oai_embed_batch.md)
leverages.

``` r
texts_to_embed <- c(
  "First document about machine learning",
  "Second document about deep learning",
  "Third document about neural networks",
  "Fourth document about data science"
)

batch_embeddings <- oai_embed_batch(
  texts = texts_to_embed,
  model = "text-embedding-3-small",
  dimensions = 1536,  # default for this model
  batch_size = 10,    # texts per API request
  concurrent_requests = 3  # parallel requests
)


batch_embeddings |>
  reframe(
    total = n(),
    succeeded = sum(!.error),
    failed = sum(.error)
  )
```

### Data Frame Integration

``` r
embedded_df <- oai_embed_df(
  df = sample_texts,
  text_var = text,
  id_var = id,
  model = "text-embedding-3-large",  # higher quality embeddings
  dimensions = 3072,  # maximum dimensions for this model
  batch_size = 20,
  concurrent_requests = 5
)

# Extract just the embeddings for downstream use
embedded_df |> 
  select(starts_with("V")) 
```

## Handling Sequence Length

### OpenAI Limits

OpenAI has a token limit of 8,192 per request. Since 1 token $\approx$ 4
characters:

``` r
long_texts <- tibble(
  id = 1:3,
  text = c(
    paste(rep("word", 100), collapse = " "),    # ~400 chars, safe
    paste(rep("word", 8000), collapse = " "),   # ~32k chars, near limit  
    paste(rep("word", 10000), collapse = " ")   # ~40k chars, too long!
  )
)

long_texts |>
  mutate(
    char_count = nchar(text),
    approx_tokens = char_count / 4,
    will_fail = approx_tokens > 8192
  )
```

We can truncate our texts with the substr() function but in practice we
would of course want to use a more intelligent splitting procedure.

``` r
# truncation (data / information loss happens!)
long_texts |>
  mutate(
    text = ifelse(nchar(text) > 32000, 
                  substr(text, 1, 32000), 
                  text)
  ) |>
  mutate(
    char_count = nchar(text),
    approx_tokens = char_count / 4,
    will_fail = approx_tokens > 8192
  )
```

### Hugging Face Limits

When using the Inference API, limits vary by model. Check the model’s
documentation. Most models handle ~512 tokens well. More modern models
can handle more (check model card). Dedicated Inference Endpoints will
receive as many requests as the assigned hardware is able to handle.

This code chunk shows you how to chunk up your texts if you’re finding
errors due to payload size:

``` r
chunk_text <- function(text, max_chars = 2000) {
  if (nchar(text) <= max_chars) return(list(text))
  
  # v. simple chunking (consider sentence boundaries/ more intelligent chunking in production)
  chunks <- substring(text, 
                     seq(1, nchar(text), max_chars), 
                     seq(max_chars, nchar(text) + max_chars - 1, max_chars))
  as.list(chunks[nchar(chunks) > 0])
}
```

## Best Practices

### Error Handling

Always check for errors in your results. This chunk shows you how to
send off your failures in another batch request, but beware you’ll need
to handle the resulting data frames.

``` r
results <- oai_embed_batch(texts = texts_to_embed)

# Check overall success
if (any(results$.error)) {
  failed <- results |> 
    filter(.error) |>
    select(text, .error_msg)
  
  print(failed)
  
  # Retry failed texts with adjusted parameters
  retry_texts <- failed$text
  retry_results <- oai_embed_batch(
    texts = retry_texts,
    batch_size = 1,  # one at a time
    timeout = 30     # longer timeout
  )
}
```

### Performance Tips

1.  **Start small**: Begin with `batch_size = 5` and
    `concurrent_requests = 1`.
2.  **Scale gradually**: Increase parameters whilst monitoring errors
3.  **Model selection**:
    - Hugging Face: `all-MiniLM-L6-v2` for speed (384 dims)
    - OpenAI: `text-embedding-3-small` with custom dimensions for
      flexibility
4.  **Consider dedicated endpoints** for production Hugging Face
    deployments

> **TIP**: Check your organisation’s tier on OpenAI, tier 5 organisatons
> can send many more requests than tier 1. [OpenAI Rate
> Limits](https://platform.openai.com/settings/organization/limits)

### Cost Optimisation

OpenAI: Reduce dimensions to save storage and computation

``` r
compact_embeddings <- oai_embed_batch(
  texts = texts_to_embed,
  model = "text-embedding-3-small",
  dimensions = 360
)
```

> **WARNING**: You’ll need to compare performance vs size trade-off for
> your particular use-case.

## Common Use Cases

### Semantic Search

A very basic implementation of semantic search (AKA dense
embedding/vector search AKA neural search )

1.  Embed your document corpus
2.  Embed search query
3.  Find similar documents (cosine similarity)
4.  Extract top 5 most similar

TODO: add code or not?

> **NOTE**: For most use-cases a hybrid approach comprising full-text
> search and semantic search will yield the best result

### Clustering

1.  Generate embeddings
2.  Extract embeddings matrix for clustering algorithm
3.  Run clustering algorithm
4.  Add clusters to data frame

``` r
embeddings_for_clustering <- hf_embed_df(
  df = sample_texts,
  text_var = text,
  id_var = id,
  endpoint_url = embed_url,
  key_name = "HF_TEST_API_KEY"
)

embedding_matrix <- embeddings_for_clustering |>
  select(starts_with("V")) |>
  as.matrix()

kmeans_result <- kmeans(embedding_matrix, centers = 2)

clustered_texts <- sample_texts |>
  mutate(cluster = kmeans_result$cluster)
```

> **TIP**: In practice you need to inspect the outputs of the clustering
> model and tune it. The code to run the model is only a small part of
> the modelling process

## Troubleshooting

### Rate Limits

Use fewer concurrent_requests if you’re running into rate limit issues.

``` r
results <- hf_embed_batch(
  texts = large_text_collection,
  batch_size = 3,
  concurrent_requests = 1  # sequential processing
)

results <- oai_embed_batch(
  texts = large_text_collection,
  batch_size = 5, # fewer requests with larger batch size
  concurrent_requests = 2
)
```

Some rate limits are by request, others are by tokens. If you’re running
into token limits, the solution is to wait longer between requests. If
you’re running into request limits, you could increase batch_size, to
embed more data with fewer requests.

### Timeouts

Increase value of timeout parameter if sending many requests, or
responses begin timing out.

``` r

results <- oai_embed_batch(
  texts = texts_to_embed,
  timeout = 60, 
  max_retries = 5
)
```

### Memory Issues

Process in chunks for very large datasets

``` r

library(purrr)

text_chunks <- split(large_text_vector, 
                     ceiling(seq_along(large_text_vector) / 1000))

all_embeddings <- map(text_chunks, ~{
  oai_embed_batch(.x, batch_size = 10)
}) |>
  list_rbind()
```

> **TIP**: You could also write your splits to individual files, and
> iterate through the files to avoid reading your data into memory all
> at once.

## Next Steps

- See the [Improving
  Performance](https://jpcompartir.github.io/EndpointR/articles/improving_performance.md)
  vignette for optimisation tips
- Check out [Hugging Face
  Inference](https://jpcompartir.github.io/EndpointR/articles/hugging_face_inference.md)
  for classification tasks
- Explore different embedding models for your specific use case

> **Remember**: embeddings are the foundation for many NLP applications.
> Choose your provider based on your needs for quality, speed, cost, and
> flexibility.
