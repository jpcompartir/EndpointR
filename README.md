
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EndpointR

<!-- badges: start -->

<!-- badges: end -->

EndpointR is a ‘batteries included’, open-source R package for
connecting to various Application Programming Interfaces
(<a href="https://en.wikipedia.org/wiki/API" target="_blank">APIs</a>)
for Machine Learning model predictions.

> **TIP:** If you are an experienced programmer, or have experience with
> hitting APIs, consider going directly to
> [httr2](https://httr2.r-lib.org/reference/index.html)

# Installation

EndpointR will not be put on CRAN, so you can download and install the
latest development version with the following code:

``` r
library(EndpointR)
remotes::install_github("jpcompartir/EndpointR")
```

# Quick Starts

## Hugging Face - embeddings

Securely set your API key

``` r
set_api_key("HF_API_KEY")
```

Point to an endpoint - this is for the ‘all-mpnet-base-v2’ model with
feature extraction (embeddings)

``` r
endpoint_url <- "https://router.huggingface.co/hf-inference/models/sentence-transformers/all-mpnet-base-v2/pipeline/feature-extraction" 
```

Embed a single text:

``` r
 hf_embed_text(
  text = "Convert this text to embeddings",
  endpoint_url = endpoint_url,
  key_name = "HF_API_KEY"
)
```

Embed a list of texts in batches:

``` r
review_texts <-c(
    "Absolutely fantastic service! The staff were incredibly helpful and friendly.",
    "Terrible experience. Food was cold and the waiter was rude.",
    "Pretty good overall, but nothing special. Average food and service.",
    "Outstanding meal! Best restaurant I've been to in years. Highly recommend!",
    "Disappointed with the long wait times. Food was okay when it finally arrived."
  )

hf_embed_batch(
  texts = review_texts,
  endpoint_url = endpoint_url,
  key_name = "HF_API_KEY",
  batch_size = 3,
  concurrent_requests = 2
)
```

Embed a data frame of texts:

``` r
review_data <- tibble::tibble(
  review_id = 1:5,
  review_text = review_texts
)
```

``` r
hf_embed_df(
  df = review_data,
  text_var = review_text,
  id_var = review_id,
  endpoint_url = endpoint_url,
  key_name = "HF_API_KEY",
  output_dir = "embeddings_output",  # writes .parquet chunks to this directory
  chunk_size = 5000,  # process 5000 rows per chunk
  concurrent_requests = 2,
  max_retries = 5,
  timeout = 15
)
```

## Hugging Face - Classification

Select a Classification Endpoint URL

``` r
sentiment_endpoint <- "https://router.huggingface.co/hf-inference/models/cardiffnlp/twitter-roberta-base-sentiment"
```

Classify a single text:

You’ll need to grab the label2id mapping from the model’s card: [Cardiff
NLP model
info](https://huggingface.co/cardiffnlp/twitter-roberta-base-sentiment/blob/main/README.md)

``` r
labelid_2class <- function() {
  return(list(negative = "LABEL_0",
              neutral = "LABEL_1",
              positive = "LABEL_2"))
}

 hf_classify_text(
  text = review_texts[[1]],
  endpoint_url = sentiment_endpoint,
  key_name = "HF_API_KEY"
) |> 
   dplyr::rename(!!!labelid_2class())
```

Classify a data frame:

``` r
hf_classify_df(
  df = review_data,
  text_var = review_text,
  id_var = review_id,
  endpoint_url = sentiment_endpoint,
  key_name = "HF_API_KEY",
  max_length = 512,  # truncate texts longer than 512 tokens
  output_dir = "classification_output",  # writes .parquet chunks to this directory
  chunk_size = 2500,  # process 2500 rows per chunk
  concurrent_requests = 3,
  max_retries = 5,
  timeout = 60
) |>
  dplyr::rename(!!!labelid_2class())
```

Read the [Hugging Face Inference
Vignette](articles/hugging_face_inference.html) for more infromation on
embedding and classifying using Dedicated Inference Endpoints and the
Inference API from Hugging Face.

## OpenAI - Chat Completions API

Make sure you’ve set your API key:

``` r
set_api_key("OPENAI_API_KEY")
```

Complete a single text:

``` r
oai_complete_text(
  text = review_texts[[2]],
  system_prompt = "Classify the sentiment of the following text: "
)
```

Complete a single text with a schema and tidy:

``` r
sentiment_schema <- create_json_schema(
  name = "sentiment_analysis",
  schema = schema_object(
    sentiment = schema_string("positive, negative, or neutral"),
    confidence = schema_number("confidence score between 0 and 1"), # we don't necessarily recommend asking a model for its confidence score, this is mainly a schema-construction demo!
    required = list("sentiment", "confidence")
  )
)

oai_complete_text(
  text = review_texts[[2]],
  system_prompt = "Classify the sentiment of the following text: ",
  schema = sentiment_schema,
  tidy = TRUE
) |> 
  tibble::as_tibble()
```

Complete a Data Frame of texts:

``` r
oai_complete_df(
  df = review_data,
  text_var = review_text,
  id_var = review_id,
  system_prompt = "Classify the following review:",
  key_name = "OPENAI_API_KEY",
  output_dir = "completions_output",  # writes .parquet chunks to this directory
  chunk_size = 1000,  # process 1000 rows per chunk
  concurrent_requests = 5,  # send 5 rows of data simultaneously
  max_retries = 5,
  timeout = 30
)
```

Complete a Data Frame of texts with schema:

``` r
df_output_w_schema <- oai_complete_df(
  df = review_data,
  text_var = review_text,
  id_var = review_id,
  system_prompt = "Classify the following review:",
  schema = sentiment_schema,
  key_name = "OPENAI_API_KEY",
  output_dir = NULL,
  # output_dir = "completions_output",
  chunk_size = 1000,
  concurrent_requests = 5
)

df_output_w_schema |> 
  dplyr::mutate(content = purrr::map(content, safely_from_json)) |>
  tidyr::unnest_wider(content)
```

# Working with Output Files

## Reading Results from Disk

Hugging Face functions (`hf_embed_df()`, `hf_classify_df()`) write
intermediate results as `.parquet` files in the specified `output_dir`.
To read all results back:

``` r
# List all parquet files (excludes metadata.json automatically)
parquet_files <- list.files("embeddings_output",
                           pattern = "\\.parquet$",
                           full.names = TRUE)

# Read all chunks into a single data frame
results <- arrow::open_dataset(parquet_files, format = "parquet") |>
  dplyr::collect()
```

## Understanding metadata.json

Each Hugging Face output directory contains a `metadata.json` file that
records:

- `endpoint_url`: The API endpoint used
- `chunk_size`: Number of rows processed per chunk
- `n_texts`: Total number of texts processed
- `concurrent_requests`: Parallel request setting
- `timeout`: Request timeout in seconds
- `max_retries`: Maximum retry attempts
- `inference_parameters`: Model-specific parameters (e.g., truncate,
  max_length)
- `timestamp`: When the job was run
- `key_name`: Which API key was used

This metadata is useful for:

- Debugging failed runs
- Reproducing results with the same settings
- Tracking which endpoint/model was used
- Understanding performance characteristics

``` r
metadata <- jsonlite::read_json("embeddings_output/metadata.json")

# check which endpoint was used
metadata$endpoint_url
```

**Note:** Add output directories to `.gitignore` to avoid committing API
responses and metadata.

Read the [LLM Providers Vignette](articles/llm_providers.html), and the
[Structured Outputs
Vignette](articles/structured_outputs_json_schema.html) for more
information on common workflows with the OpenAI Chat Completions API
[^1]

# API Key Security

- Read the
  <a href="https://httr2.r-lib.org/articles/wrapping-apis.html#basics"
  target="_blank">httr2 vignette</a> on managing your API keys securely
  and encrypting them.

- Read the [EndpointR API Keys](articles/api_keys.html) vignette for
  information on which API keys you need for wach endpoint we support,
  and how to securely import those API keys into your .Renvironfile.

[^1]: Content pending implementation for Anthroic Messages API, Gemini
    API, and OpenAI Responses API
