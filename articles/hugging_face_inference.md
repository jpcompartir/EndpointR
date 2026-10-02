# Using Hugging Face Inference Endpoints

This vignette shows how to embed and classify text with EndpointR using
Hugging Face’s inference services.

## Setup

``` r

library(EndpointR)
library(dplyr)
library(httr2)
library(tibble)
library(arrow)

my_data <- tibble(
  id = 1:3,
  text = c(
    "Machine learning is fascinating",
    "I love working with embeddings",
    "Natural language processing is powerful"
  ),
  category = c("ML", "embeddings", "NLP")
)
```

Follow Hugging Face’s
[docs](https://huggingface.co/docs/hub/security-tokens) to generate a
Hugging Face token, and then register it with EndpointR:

``` r

set_api_key("HF_TEST_API_KEY")
```

## Choosing Your Service

Hugging Face offers two inference options:

- **Inference API**: Free, good for testing
- **Dedicated Endpoints**: Paid, reliable, fast

For this vignette, we’ll use the Inference API. To switch to dedicated
endpoints, just change the URL.

## Getting Started

Go to [Hugging Face’s models hub](https://huggingface.co/models) and
fetch the Inference API’s URL for the model you want to embed your data
with. Not all models are available via the Hugging Face Inference API,
if you need to use a model that is not available you may need to deploy
a [Dedicated Inference
Endpoint](https://huggingface.co/inference-endpoints/dedicated).

## Which engine does my endpoint run?

Hugging Face endpoints run one of two inference engines, and the two
engines need different request formats. EndpointR checks which engine an
endpoint runs, so in most cases you don’t need to do anything. It still
helps to know the differences, because they change how texts are cut,
how fast a job runs, and whether you get the same scores twice.

- [Text Embeddings
  Inference](https://huggingface.co/docs/text-embeddings-inference)
  (TEI) is a server for embedding models and classifiers. Hugging Face
  picks it by default for most embedding models, and you can choose it
  for classifiers.
- The default Hugging Face Inference Toolkit runs a `transformers`
  pipeline. Endpoints that don’t use TEI, or another named container,
  run the toolkit.

|  | Default toolkit | TEI |
|----|----|----|
| Batch of texts, embeddings | `{"inputs": ["a", "b"]}` | `{"inputs": ["a", "b"], "truncate": true}` |
| Batch of texts, classification | `{"inputs": ["a", "b"], "parameters": {...}}` | `{"inputs": [["a"], ["b"]], "truncate": true, "raw_scores": true}` |
| Batching on the GPU | Only if the request sets `parameters$batch_size`, which EndpointR does. The default is 1 text at a time. | Always. TEI combines texts from all waiting requests into one batch. |
| Texts per request | No fixed limit | `--max-client-batch-size`, 32 by default. Larger requests are rejected. |
| Truncation | `parameters$truncation` and `parameters$max_length` | Top-level `truncate`, which cuts at the model’s own limit (8,192 tokens for ModernBERT and BGE M3). TEI ignores `max_length`. |
| Sorting texts by length | Helps, because the pipeline pads each batch to its longest text (about 20% faster) | No effect, because TEI does not pad |
| More requests in flight | No gain above about 8 | Helps up to about 32 |
| Precision | fp32 | fp16 |
| Repeatable results | Yes | No, see [Repeatable results](#repeatable-results) |
| `GET /info` route | None | Returns the model and its limits |

### How EndpointR detects the engine

Every `hf_embed_*()` and `hf_classify_*()` function has an `engine`
argument. The default, `"auto"`, sends `GET {endpoint_url}/info` once
per endpoint URL in each R session and keeps the answer. TEI answers
with JSON that includes `max_client_batch_size`, and the toolkit has no
`/info` route, so it answers with an error such as 404. The call retries
503 responses for about 2 minutes, because an endpoint that has scaled
to zero returns 503 while it starts.

You can skip the check with `engine = "tei"` or `engine = "toolkit"`,
e.g. when a proxy blocks `/info`. To set it for a whole session, use an
option:

``` r

options(EndpointR.hf_engine = "tei")
```

To see what an endpoint reports, call
[`hf_get_endpoint_info()`](https://jpcompartir.github.io/EndpointR/reference/hf_get_endpoint_info.md).
It returns the `/info` JSON on TEI, and `NULL` with a message on the
toolkit:

``` r

info <- hf_get_endpoint_info(
  endpoint_url = "https://your-endpoint.endpoints.huggingface.cloud",
  key_name = "HF_API_KEY"
)

info$max_client_batch_size # texts allowed per request
info$max_input_length      # tokens per text before TEI cuts it
info$version
```

If you have the [`hf` command line
tool](https://huggingface.co/docs/huggingface_hub/guides/cli),
`hf endpoints describe <name>` shows the engine too. An
`"image": {"tei": ...}` entry means TEI, and an
`"image": {"huggingface": {}}` entry means the toolkit.

EndpointR records the engine, and on TEI the `/info` fields, in the
`metadata.json` file of every `_chunks()` and `_df()` run.

## Understanding the Function Hierarchy

EndpointR provides four levels of functions for working with Hugging
Face endpoints.

> **KEY FEATURE**: The `*_df()` and `*_chunks()` functions preserve your
> original column names. If you pass a data frame with columns named
> `review_id` and `review_text`, those exact names will appear in the
> output and in the saved `.parquet` files. This makes it easy to join
> results back to your original data.

### Single Text Functions

- [`hf_embed_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_text.md) -
  Embed a single text
- [`hf_classify_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_text.md) -
  Classify a single text

Use these for one-off requests or testing.

### Batch Functions

- [`hf_embed_batch()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_batch.md) -
  Embed multiple texts in memory
- [`hf_classify_batch()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_batch.md) -
  Classify multiple texts in memory

Use these for small to medium datasets (\<5000 texts) that fit in
memory. Results are returned as a single data frame.

### Chunk Functions (NEW in v0.1.2)

- [`hf_embed_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_chunks.md) -
  Process large volumes with incremental file writing
- [`hf_classify_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_chunks.md) -
  Process large volumes with incremental file writing

Use these for large datasets (\>5000 texts). Results are written
incrementally as `.parquet` files to avoid memory issues and provide
safety against crashes.

### Data Frame Functions

- [`hf_embed_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_df.md) -
  Convenience wrapper that calls
  [`hf_embed_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_chunks.md)
- [`hf_classify_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_df.md) -
  Convenience wrapper that calls
  [`hf_classify_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_chunks.md)

**Most users will use these.** They handle extraction from data frames
and call the chunk functions internally.

### Choosing the Right Function

Use this decision tree:

``` r

# Single text? Use _text functions
if (n_texts == 1) {
  result <- hf_embed_text(text, endpoint_url, key_name)
  # or
  result <- hf_classify_text(text, endpoint_url, key_name)
}

# Small batch (<5000 texts) and want results in memory only?
if (n_texts < 5000 && !need_file_output) {
  results <- hf_embed_batch(texts, endpoint_url, key_name,
                            batch_size = 32, concurrent_requests = 16)
  # or
  results <- hf_classify_batch(texts, endpoint_url, key_name,
                               batch_size = 32, concurrent_requests = 16)
}

# Large dataset or want file output for safety?
# Use _df functions (they call _chunks internally)
if (n_texts >= 5000 || need_safety) {
  results <- hf_embed_df(df, text, id, endpoint_url, key_name,
                         chunk_size = 5000, output_dir = "my_results",
                         batch_size = 32, concurrent_requests = 16)
  # or
  results <- hf_classify_df(df, text, id, endpoint_url, key_name,
                            chunk_size = 5000, output_dir = "my_results",
                            batch_size = 32, concurrent_requests = 16,
                            max_length = 512)
}
```

> **Recommendation**: For most production use cases, use `_df` functions
> even for smaller datasets. The safety of incremental file writing is
> worth it.

### How the batch, chunk and data frame functions send texts

The `_batch()`, `_chunks()` and `_df()` functions send `batch_size`
texts in each request (32 by default) and keep `concurrent_requests`
requests in flight (16 by default). Before 0.3.0, the `_chunks()` and
`_df()` functions sent one text per request and `_df()` sent one request
at a time, which left the GPU mostly idle. In our tests, the new
defaults were more than 100 times faster on TEI. The [Improving
Performance](https://jpcompartir.github.io/EndpointR/articles/improving_performance.md)
vignette has the numbers.

The functions also do the following:

- They don’t send empty or missing texts. Those rows come back with
  `.error = TRUE` and the message “Empty or missing text, not sent”, so
  the output has one row per input.
- On the toolkit, they sort texts by length before they make batches.
  Results always come back in the order of your input.
- When a batch fails with HTTP 400, 413, 422 or 424, a network error, or
  the wrong number of results, they split it in half and send each half
  again, down to single texts. Only the text at fault fails.
- When a request gets HTTP 429, 502, 503 or 504, they wait and send it
  again unchanged, up to `max_retries` times.
- On TEI, when `batch_size` is above the endpoint’s
  `max_client_batch_size`, they lower `batch_size` to that value and
  warn you.

## Key Differences: Embeddings vs Classification

The request EndpointR sends depends on the task and on the engine.
EndpointR handles the differences, but they explain some of the
arguments and results.

### Text Truncation Handling

**Embeddings** (`hf_embed_*`):

- There is no `max_length` argument.
- On TEI, EndpointR sends `truncate: true` at the top level of the
  request, so TEI cuts each text at the model’s own limit (e.g. 8,192
  tokens for BGE M3). You no longer need `AUTO_TRUNCATE=true` on the
  endpoint, although it does no harm.
- On the toolkit, truncation depends on the model’s pipeline. None of
  our embedding endpoints run the toolkit, so this path is untested.

**Classification** (`hf_classify_*`):

- `max_length` (default `512L`) sets the maximum number of tokens per
  text.
- On the toolkit, EndpointR sends `truncation: true` and `max_length` to
  the endpoint, and the pipeline cuts each text.
- On TEI, EndpointR cuts texts in R before it sends them, because TEI
  ignores `max_length` and only cuts at the model’s own limit.

### Why EndpointR cuts classifier texts in R on TEI

Long texts give NaN scores on some TEI classifiers. TEI runs models in
fp16 (half precision), and our ModernBERT spam classifier returned NaN
scores for many long texts. The same texts failed when sent on their
own, so the cause is length, not batching.

| Text length           | Texts with a NaN score |
|-----------------------|------------------------|
| Up to 2,048 tokens    | 0 of 10,877            |
| 2,049 to 4,096 tokens | 9 of 683               |
| 4,097 to 8,192 tokens | 53 of 143              |
| Above 8,192 tokens    | 54 of 54               |

A NaN score also crashed TEI 1.8.2 when the request asked TEI for
probabilities. TEI’s softmax failed, the endpoint returned 503, and it
then stayed in a failed state until someone restarted it. EndpointR
avoids the crash because it sends `raw_scores: true` and applies the
softmax in R (see
[`tidy_tei_classification_response()`](https://jpcompartir.github.io/EndpointR/reference/tidy_tei_classification_response.md)).
With raw scores, TEI returns `424 {"error": "score is NaN"}` for the
request and keeps running, and EndpointR then splits the batch so that
only the long text fails.

EndpointR cuts texts in one of two ways:

1.  If the [`tok`](https://cran.r-project.org/package=tok) package is
    installed and EndpointR knows the model id, it downloads the model’s
    `tokenizer.json` from the Hugging Face Hub (with your `key_name`, so
    private models work). It then cuts each text longer than
    `max_length` tokens to the text covered by its first `max_length`
    tokens. Dedicated endpoints report their model id as `/repository`,
    so pass the id with the `tokenizer` argument.
2.  Otherwise, it cuts each text to `max_chars` characters (2,000 by
    default) and prints a message. The tokeniser works on bytes, so some
    scripts produce more than one token per character, and 2,000
    characters is not a guaranteed limit of 512 tokens.

``` r

install.packages("tok")

hf_classify_df(
  df = my_data,
  text_var = text,
  id_var = id,
  endpoint_url = classify_url,
  key_name = "HF_API_KEY",
  max_length = 512,
  tokenizer = "org/model-name"  # the model id on the Hugging Face Hub
)
```

Set `max_length = NULL` to send texts uncut. `metadata.json` records the
method in `truncation_method` (`"tok"`, `"max_chars"`, `"none"`, or
`"endpoint"` on the toolkit) and the number of texts that were cut in
`n_texts_cut`. The output’s text column always holds your original
texts.

### Inference Parameters Sent to API

The request body depends on the engine and the task. EndpointR always
sends the texts as a JSON array, even for a batch of one text.

**Embeddings on TEI**:

``` json
{
  "inputs": ["first text", "second text"],
  "truncate": true
}
```

**Classification on TEI**:

``` json
{
  "inputs": [["first text"], ["second text"]],
  "truncate": true,
  "raw_scores": true
}
```

Each text is wrapped in its own list, because TEI reads a flat list of
exactly 2 texts as one sentence pair. It returns one result with no
error, so 2 texts would get 1 score. TEI rejects a flat list of 3 or
more texts.

**Classification on the toolkit**:

``` json
{
  "inputs": ["first text", "second text"],
  "parameters": {
    "return_all_scores": true,
    "truncation": true,
    "max_length": 512,
    "batch_size": 2
  }
}
```

`batch_size` is set to the number of texts in the request. Without it,
the toolkit pipeline runs one text at a time on the GPU, even when a
request holds many texts.

The `parameters` argument of the `hf_*` functions adds to or overrides
these defaults. On TEI, EndpointR adds the entries to the top level of
the body, because TEI ignores a `parameters` field. Check
`metadata.json` (see below) to see what was sent.

## Embeddings

### Single Text

Embed one piece of text:

``` r

# inference api url for embeddings
embed_url <- "https://router.huggingface.co/hf-inference/models/sentence-transformers/all-mpnet-base-v2/pipeline/feature-extraction"

result <- hf_embed_text(
  text = "This is a sample text to embed",
  endpoint_url = embed_url,
  key_name = "HF_API_KEY"
)
```

The result is a tibble with one row and 384 columns (V1 to V384). Each
column is an embedding dimension.

> **Note**: The number of columns depends on your model. Check the
> model’s Hugging Face page for its embedding size.

### List of Texts

Embed multiple texts at once using batching:

``` r

texts <- c(
  "First text to embed",
  "Second text to embed",
  "Third text to embed"
)

batch_result <- hf_embed_batch(
  texts,
  endpoint_url = embed_url,
  key_name = "HF_API_KEY",
  batch_size = 32,          # texts per request
  concurrent_requests = 16  # requests in flight at once
)
```

The result includes:

- `text`: your original text
- `.error`: TRUE if something went wrong
- `.error_msg`: what went wrong (if anything)
- `.status`: the HTTP status code of a failed request
- `V1` to `V384`: the embedding values

### Processing Data Frames with Chunk Writing

Most commonly, you’ll want to embed a column in a data frame. The
[`hf_embed_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_df.md)
function processes data in chunks and writes intermediate results to
disk.

#### Understanding output_dir

Both
[`hf_embed_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_df.md)
and
[`hf_classify_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_df.md)
write intermediate results to disk as `.parquet` files. This provides:

1.  **Safety**: If your job crashes, you don’t lose all progress
2.  **Memory efficiency**: Large datasets don’t overwhelm your RAM
3.  **Reproducibility**: Metadata tracks exactly what parameters you
    used

``` r

# Basic usage - auto-generates output directory
embedding_result <- hf_embed_df(
  df = my_data,
  text_var = text,      # column with your text
  id_var = id,          # column with unique ids
  endpoint_url = embed_url,
  key_name = "HF_API_KEY",
  output_dir = "auto",  # Creates "hf_embeddings_batch_TIMESTAMP"
  chunk_size = 5000,    # Writes every 5000 rows
  batch_size = 32,      # Texts per request
  concurrent_requests = 16
)

# Custom output directory
embedding_result <- hf_embed_df(
  df = my_data,
  text_var = text,
  id_var = id,
  endpoint_url = embed_url,
  key_name = "HF_API_KEY",
  output_dir = "my_embeddings_v1",  # Your custom directory name
  chunk_size = 5000
)
```

#### Output Directory Structure

After running
[`hf_embed_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_df.md)
or
[`hf_classify_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_df.md),
you’ll have:

    my_embeddings_v1/
    ├── chunk_001.parquet
    ├── chunk_002.parquet
    ├── chunk_003.parquet
    └── metadata.json

**IMPORTANT**: Add your output directories to `.gitignore`! These files
contain API responses and can be large.

``` r
# .gitignore
hf_embeddings_batch_*/
hf_classification_chunks_*/
my_embeddings_v1/
```

#### Reading Results from Disk

If your R session crashes or you want to reload results later:

``` r

# List all parquet files (excludes metadata.json automatically)
parquet_files <- list.files("my_embeddings_v1",
                           pattern = "\\.parquet$",
                           full.names = TRUE)

# Read all chunks into a single data frame
results <- arrow::open_dataset(parquet_files, format = "parquet") |>
  dplyr::collect()

# Check for any errors
results |> count(.error)

# Extract only successful embeddings
successful <- results |> filter(.error == FALSE)
```

#### Understanding metadata.json

The metadata file records everything about your processing job:

``` r

metadata <- jsonlite::read_json("my_embeddings_v1/metadata.json")

# Check which endpoint was used
metadata$endpoint_url

# Check which engine the endpoint runs, and its limits on TEI
metadata$engine
metadata$max_client_batch_size
metadata$version

# See processing parameters
metadata$chunk_size
metadata$batch_size
metadata$concurrent_requests
metadata$timeout

# See how many texts were empty, and how many batches were split
metadata$n_empty_texts
metadata$n_split_batches

# For classification, see the request body without the texts, and how texts were cut
metadata$inference_parameters
metadata$truncation_method
metadata$n_texts_cut

# Check when the job ran
metadata$timestamp
```

This metadata is invaluable for:

- Debugging why a job failed
- Reproducing results with identical settings
- Tracking which model/endpoint version was used
- Understanding performance characteristics

#### Check for Errors

Always verify your results:

``` r

embedding_result |> count(.error)

# View any failures (column names match your original data frame)
failures <- embedding_result |>
  filter(.error == TRUE) |>
  select(id, .error_msg)

# Extract just the embeddings for successful rows
embeddings_only <- embedding_result |>
  filter(.error == FALSE) |>
  select(starts_with("V"))
```

## Classification

Classification works similarly to embeddings, but with a different URL,
output format, and the additional `max_length`, `max_chars` and
`tokenizer` arguments for controlling text truncation.

### Single Text

``` r

classify_url <- "https://router.huggingface.co/hf-inference/models/distilbert/distilbert-base-uncased-finetuned-sst-2-english"

sentiment <- hf_classify_text(
  text = "I love this package!",
  endpoint_url = classify_url,
  key_name = "HF_API_KEY"
)
```

### Processing Data Frames

``` r

classification_result <- hf_classify_df(
  df = my_data,
  text_var = text,
  id_var = id,
  endpoint_url = classify_url,
  key_name = "HF_API_KEY",
  max_length = 512,  # Truncate texts longer than 512 tokens
  output_dir = "my_classification_v1",
  chunk_size = 5000,
  batch_size = 32,
  concurrent_requests = 16,
  timeout = 120
)
```

The result includes:

- Your original ID and text columns (with their original names
  preserved)
- One column per classification label (e.g., POSITIVE, NEGATIVE),
  holding that label’s probability
- Error tracking columns (`.error`, `.error_msg`, `.status`)
- Chunk tracking (`.chunk`)

> **NOTE**: Classification labels are model and task specific. Check the
> model card on Hugging Face for label mappings.

> **NOTE**: On TEI, EndpointR asks for raw scores (logits) and turns
> them into probabilities in R with
> [`tidy_tei_classification_response()`](https://jpcompartir.github.io/EndpointR/reference/tidy_tei_classification_response.md).
> On the toolkit, it uses `tidy_batch_classification_response()`. If you
> pass your own `tidy_func`, it receives the response for one batch and
> must return one row per text.

> **IMPORTANT**: The function preserves your original column names. If
> your data frame has `review_id` and `review_text`, those names will
> appear in the output, not generic `id` and `text`.

### Renaming Classification Labels

Many classification models use generic labels like `LABEL_0`, `LABEL_1`.
You can rename these:

``` r

# Create a mapping function
labelid_2class <- function() {
  return(list(
    negative = "LABEL_0",
    neutral = "LABEL_1",
    positive = "LABEL_2"
  ))
}

# Apply the mapping
classification_result <- hf_classify_df(
  df = my_data,
  text_var = text,
  id_var = id,
  endpoint_url = classify_url,
  key_name = "HF_API_KEY",
  max_length = 512
) |>
  dplyr::rename(!!!labelid_2class())
```

## Utility Functions

EndpointR provides utility functions to help you work with Hugging Face
endpoints.

### Get Model Token Limits

Find out the maximum token length for a model:

``` r

# Get the model's max token length from Hugging Face
max_tokens <- hf_get_model_max_length(
  model_name = "cardiffnlp/twitter-roberta-base-sentiment",
  api_key = "HF_API_KEY"
)

# Use this to set max_length for classification
hf_classify_df(
  df = my_data,
  text_var = text,
  id_var = id,
  endpoint_url = classify_url,
  key_name = "HF_API_KEY",
  max_length = max_tokens  # Use the model's actual limit
)
```

This is especially useful when working with different models that have
varying token limits (e.g., 512, 1024, 2048).

### Get Endpoint Information

Retrieve detailed information about a Dedicated Inference Endpoint that
runs TEI. For endpoints that run the toolkit,
[`hf_get_endpoint_info()`](https://jpcompartir.github.io/EndpointR/reference/hf_get_endpoint_info.md)
returns `NULL` with a message, because the toolkit has no `/info` route:

``` r

endpoint_info <- hf_get_endpoint_info(
  endpoint_url = "https://your-endpoint.endpoints.huggingface.cloud",
  key_name = "HF_API_KEY"
)

# Check endpoint configuration
endpoint_info
```

This is useful for:

- Checking which engine an endpoint runs
- Checking `max_client_batch_size` before you raise `batch_size`
- Checking the TEI version and the model’s token limit

## Using Dedicated Endpoints

To use dedicated endpoints instead of the Inference API:

1.  Deploy your model to a dedicated endpoint (see [Hugging Face
    docs](https://huggingface.co/docs/inference-endpoints))
2.  Get your endpoint URL
3.  Replace the URL in any function:

``` r

# just change this line
dedicated_url <- "https://your-endpoint-name.endpoints.huggingface.cloud"

# everything else stays the same
result <- hf_embed_text(
  text = "Sample text",
  endpoint_url = dedicated_url,  # <- only change
  key_name = "HF_API_KEY"
)
```

> **Note**: A dedicated endpoint that has scaled to zero returns 503 for
> about 1.5 to 2 minutes while it starts. With `engine = "auto"`,
> EndpointR’s `/info` check waits for up to about 2 minutes, so the
> endpoint is usually ready before the first batch goes out.

### Setting AUTO_TRUNCATE for Embedding Endpoints

Since 0.3.0, EndpointR sends `truncate: true` with every embedding
request to a TEI endpoint, so TEI cuts long texts at the model’s limit.
Earlier versions sent `truncate` inside `parameters`, which TEI ignores,
and relied on the endpoint setting.

You can still set the environment variable `AUTO_TRUNCATE=true` in your
endpoint settings on Hugging Face, so that requests from other tools are
cut too.

## Tips and Best Practices

### Performance Tuning

- **Use the defaults**: `batch_size = 32` and `concurrent_requests = 16`
  work on every TEI endpoint, because 32 is TEI’s default
  `max_client_batch_size`
- **Raise `batch_size` on TEI if the endpoint allows it**: check
  `max_client_batch_size` with
  [`hf_get_endpoint_info()`](https://jpcompartir.github.io/EndpointR/reference/hf_get_endpoint_info.md).
  On our spam classifier, set to 128, 128 texts per request and 32
  requests in flight gave about 4,300 texts per second
- **Don’t go above 64 texts per request on the toolkit**: 64 was the
  best setting in our tests, and larger batches were slower
- **Watch your rate limits**:
  - Inference API: Shared limits, reduce concurrency if you hit errors
  - Dedicated Endpoints: Limited by hardware, not API rate limits

### Memory Management

- Use `chunk_size` to control memory usage
- Smaller chunks = more frequent disk writes = less memory needed
- For very large datasets (\>100k rows), use `chunk_size = 1000-2500`

``` r

# For very large datasets
hf_embed_df(
  df = large_data,
  text_var = text,
  id_var = id,
  endpoint_url = embed_url,
  key_name = "HF_API_KEY",
  chunk_size = 1000  # Smaller chunks for memory efficiency
)
```

### Truncation Strategy

**For Embeddings**:

1.  On TEI, EndpointR sends `truncate: true`, so TEI cuts texts at the
    model’s limit
2.  For Inference API, truncation is handled automatically by most
    models
3.  Consider preprocessing very long texts before embedding (e.g., take
    first N characters)

**For Classification**:

1.  Use
    [`hf_get_model_max_length()`](https://jpcompartir.github.io/EndpointR/reference/hf_get_model_max_length.md)
    to check the model’s token limit
2.  Set `max_length` appropriately (default 512 works for most models)
3.  On TEI, install `tok` and pass the model id as `tokenizer`, so texts
    are cut at exactly `max_length` tokens
4.  For documents longer than `max_length`, consider:
    - Chunking documents and classifying each chunk
    - Summarization before classification
    - Using models with longer context windows

``` r

# Get model's actual max length
model_limit <- hf_get_model_max_length(
  model_name = "distilbert/distilbert-base-uncased-finetuned-sst-2-english",
  api_key = "HF_API_KEY"
)

# Use 90% of the limit to be safe
safe_limit <- as.integer(model_limit * 0.9)

hf_classify_df(
  df = my_data,
  text_var = text,
  id_var = id,
  endpoint_url = classify_url,
  key_name = "HF_API_KEY",
  max_length = safe_limit
)
```

### Error Recovery

Always check for errors and consider retrying failures:

``` r

# Check results for errors
results |> count(.error)

# Identify failed texts (column names match your input data frame)
failed <- results |> filter(.error == TRUE)

# Note: Column names below will match your original data frame
# If you used review_id and review_text, use those names instead
failed |> select(id, .error_msg)

# EndpointR has already split failed batches down to single texts, so a text
# that still fails usually fails on its own (e.g. a NaN score, HTTP 424).
# Texts that failed with a timeout or 5xx may succeed on a second attempt.
# Access text column by its actual name from your data
retry_results <- hf_embed_batch(
  texts = failed$text,  # Use your actual column name
  endpoint_url = embed_url,
  key_name = "HF_API_KEY",
  timeout = 300,    # Longer timeout
  max_retries = 10  # More retries
)
```

### Production Recommendations

1.  **Always use output_dir**: Never rely solely on in-memory results
    for large jobs
2.  **Monitor metadata**: Check `metadata.json` to verify your settings
3.  **Add to .gitignore**: Keep API responses out of version control
4.  **Use Dedicated Endpoints**: For production workloads, avoid the
    free Inference API
5.  **Set appropriate timeouts**: The default of 120 seconds allows for
    batches of long texts
6.  **Test with small samples**: Before processing 1M rows, test with
    100 rows
7.  **Monitor costs**: Track your Dedicated Endpoint usage on Hugging
    Face

## Common Issues

### “Payload too large” Errors

**For Embeddings**:

- **Dedicated Endpoints on TEI**: EndpointR sends `truncate: true`, so
  long texts are cut. If you still see the error, lower `batch_size`,
  because the whole request may be too large
- **Inference API**: Preprocess and truncate texts before sending

``` r

# Preprocessing approach for Inference API
my_data <- my_data |>
  mutate(text = substr(text, 1, 5000))  # Limit to ~5000 characters
```

**For Classification**:

- Reduce the `max_length` parameter

``` r

hf_classify_df(
  df = my_data,
  text_var = text,
  id_var = id,
  endpoint_url = classify_url,
  key_name = "HF_API_KEY",
  max_length = 256  # Reduce from default 512
)
```

### Timeouts

Batches of long texts can take longer than the default timeout of 120
seconds. When a request times out, EndpointR splits the batch and sends
the halves again. If many batches time out, increase the timeout:

``` r

hf_classify_df(
  df = my_data,
  text_var = text,
  id_var = id,
  endpoint_url = classify_url,
  key_name = "HF_API_KEY",
  timeout = 300,  # Increase from default 120
  max_retries = 10
)
```

### Dedicated Endpoint Cold Starts

A dedicated endpoint that has scaled to zero returns 503 for about 1.5
to 2 minutes while it starts. With `engine = "auto"`, EndpointR’s
`/info` check retries for about 2 minutes, so it usually waits until the
endpoint is ready. If you set `engine` yourself, the batches are retried
instead, with waits of 2, 4, 8, 16 and 30 seconds by default. Raise
`max_retries` to wait longer:

``` r

hf_embed_df(
  df = my_data,
  text_var = text,
  id_var = id,
  endpoint_url = dedicated_url,
  key_name = "HF_API_KEY",
  engine = "tei",
  max_retries = 10  # waits up to about 4 minutes in total
)
```

### Out of Memory Errors

Reduce `chunk_size`:

``` r

# Instead of default 5000
hf_embed_df(
  df = large_data,
  text_var = text,
  id_var = id,
  endpoint_url = embed_url,
  key_name = "HF_API_KEY",
  chunk_size = 1000  # Smaller chunks
)
```

### Rate Limit Errors

**For Inference API**:

- Reduce `concurrent_requests`, e.g. to 2
- EndpointR waits and retries requests that get 429

``` r

hf_embed_df(
  df = my_data,
  text_var = text,
  id_var = id,
  endpoint_url = embed_url,
  key_name = "HF_API_KEY",
  concurrent_requests = 2,
  max_retries = 10  # More retries with backoff
)
```

**For Dedicated Endpoints**:

- Not typically rate-limited
- TEI returns 429 when its queue is full, and EndpointR retries those
  requests
- If you see many errors, reduce `concurrent_requests` or upgrade your
  endpoint hardware

### Model Not Available

Not all models work with the Inference API. Check the model page on
Hugging Face. If the model isn’t available via Inference API, you’ll
need to:

1.  Deploy a Dedicated Inference Endpoint
2.  Use a different model that is available via Inference API
3.  Run the model locally (outside of EndpointR)

### Problems we have seen with TEI and the toolkit

We found the following problems when we tested our endpoints. EndpointR
handles each of them, but they help explain errors you may see from
other tools.

1.  A flat list of exactly 2 texts sent to a TEI classifier is read as
    one sentence pair, and TEI returns one result with no error. A flat
    list of 3 or more texts is rejected with
    `Failed to deserialize the JSON body ... expected a string, a pair of strings [string, string] or a batch of mixed strings and pairs`.
    EndpointR sends `[["a"], ["b"]]` to TEI classifiers.
2.  A batch of 1 must still be sent as a list. In R, `as.list(texts)`
    gives `["a"]`, but a character vector of length 1 gives `"a"`.
3.  TEI ignores `parameters$max_length`, and long texts can give NaN
    scores in fp16. See [Why EndpointR cuts classifier texts in R on
    TEI](#why-endpointr-cuts-classifier-texts-in-r-on-tei).
4.  A NaN score crashes TEI 1.8.2 when `raw_scores` is false, and the
    endpoint stays in a failed state until someone restarts it. With
    `raw_scores: true`, TEI returns 424 for the request and keeps
    running.
5.  One empty text makes the toolkit reject the whole batch with
    `You need to specify either text or text_target`. EndpointR does not
    send empty texts.
6.  TEI rejects requests with more texts than `max_client_batch_size`.
    EndpointR lowers `batch_size` to that value.
7.  A scaled-to-zero endpoint returns 503 for about 1.5 to 2 minutes
    while it starts.
8.  Some fine-tuned classifiers need a change to their `config.json`
    before they run on TEI. For our spam classifier, the `label2id`
    values had to be numbers, not strings, and we had to add
    `"num_labels": 2`.

## Repeatable results

TEI does not give exactly the same results each time you run the same
texts. TEI runs models in fp16 and combines texts from all waiting
requests into one batch, so the numbers depend on which texts share a
batch, and that depends on timing. This happens whatever the client
sends, including one text per request. The toolkit runs in fp32 and gave
the same results each time in our tests.

In our tests, the differences were small:

- When we ran the same 100,000 texts through our spam classifier on TEI
  twice, 18% of the scores changed slightly and 4 labels changed. The
  largest change in a score was 0.071.
- 99.95% of spam labels from TEI matched the labels from the same model
  on the toolkit. The 50 labels that changed all had toolkit scores
  between 0.494 and 0.557.
- BGE M3 embeddings of 50,000 messages, made on TEI with 32 texts per
  request, matched embeddings of the same messages made a year earlier
  with 1 text per request. The median cosine similarity was 0.9999993
  and the lowest was 0.99907.

The embedding differences change a UMAP and HDBSCAN clustering about as
much as a new UMAP seed does. The adjusted Rand index against the
original clustering was 0.59 to 0.66, compared with 0.65 for a new seed.
PCA followed by k-means was stable, at 0.96 to 1.00.

To make an analysis repeatable, save the embeddings and scores once,
e.g. in the `.parquet` files that the `_df()` functions write, and reuse
them. Don’t embed or classify the same texts again and expect identical
numbers.

## Improving Performance

For detailed performance optimization strategies, visit the [Improving
Performance](https://jpcompartir.github.io/EndpointR/articles/improving_performance.md)
vignette.

Quick tips:

- Start with the defaults, `batch_size = 32` and
  `concurrent_requests = 16`
- On TEI, raise `batch_size` up to the endpoint’s
  `max_client_batch_size`, and `concurrent_requests` up to about 32
- On the toolkit, use up to 64 texts per request and about 8 requests in
  flight
- Use larger `chunk_size` values for faster processing (if memory
  allows)
- For Dedicated Endpoints, upgrade hardware for better throughput
- Use batch functions
  ([`hf_embed_batch()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_batch.md),
  [`hf_classify_batch()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_batch.md))
  for small datasets to avoid file I/O overhead

## Appendix

### Comparison of Inference API vs Dedicated Inference Endpoints

| Feature | Inference API | Dedicated Inference Endpoints |
|----|----|----|
| **Accessibility** | Public, shared service | Private, dedicated hardware |
| **Cost** | Free (with paid tiers) | Paid service - rent specific hardware |
| **Hardware** | Shared computing resources | Dedicated hardware allocation |
| **Wait Times** | Variable, unknowable in advance | Predictable, ~30s for cold start |
| **Production Ready** | Not recommended for production | Recommended for production use |
| **Use Case** | Casual usage, testing, prototyping | Production applications |
| **Scalability** | Limited by shared resources | Scales with dedicated allocation |
| **Availability** | Subject to shared infrastructure limits | Guaranteed availability during rental |
| **Model Coverage** | Commonly-used models, models selected by HF | Virtually all models on the Hub |
| **Truncation Control** | Limited (model-dependent) | Full control via environment variables |
| **Engine** | Chosen by Hugging Face | TEI or the toolkit, chosen when you deploy |
