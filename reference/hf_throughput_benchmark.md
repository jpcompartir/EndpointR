# Hugging Face endpoint throughput benchmark

Texts per second for different ways of sending texts to Hugging Face
dedicated endpoints, measured on 1 October 2026. The client was a laptop
in the UK and the endpoints were A100s in AWS us-east-1. The texts were
short social media posts, about 57 to 61 tokens each.

## Usage

``` r
hf_throughput_benchmark
```

## Format

A tibble with 18 rows and 9 variables:

- model:

  Character; `modernbert-spam` (a ModernBERT base classifier) or
  `bge-m3` (an embedding model)

- engine:

  Character; `toolkit` (the default Hugging Face Inference Toolkit) or
  `tei` (Text Embeddings Inference 1.8.2)

- hardware:

  Character; the endpoint's GPU

- method:

  Character; how the texts were sent

- texts_per_request:

  Integer; texts sent in each request

- concurrent_requests:

  Integer; requests in flight at once

- sorted:

  Logical; whether texts were sorted by length before batching

- n_texts:

  Integer; number of texts in the run

- texts_per_sec:

  Numeric; end-to-end texts per second seen by the client

## Source

Benchmarks run by the EndpointR maintainers; see
`data-raw/hf_throughput_benchmark.R`
