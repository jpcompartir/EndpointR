# Batch concurrent benchmark results

Benchmark data comparing different batch sizes and concurrent request
configurations for Hugging Face API calls.

## Usage

``` r
batch_concurrent_benchmark
```

## Format

A data frame with 20 rows and 7 variables:

- batch_size:

  Integer; size of each batch processed

- concurrent_requests:

  Integer; number of concurrent requests

- chunk_index:

  Integer; index of the processed chunk

- elapsed_time:

  Numeric; time taken in seconds

- success:

  Logical; whether the operation succeeded

- rows_processed:

  Integer; number of rows successfully processed

- throughput:

  Numeric; requests processed per second

## Source

Internal benchmarking of EndpointR functions
