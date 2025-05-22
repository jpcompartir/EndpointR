#' Batch concurrent benchmark results
#'
#' Benchmark data comparing different batch sizes and concurrent request
#' configurations for Hugging Face API calls.
#'
#' @format A data frame with 20 rows and 7 variables:
#' \describe{
#'   \item{batch_size}{Integer; size of each batch processed}
#'   \item{concurrent_requests}{Integer; number of concurrent requests}
#'   \item{chunk_index}{Integer; index of the processed chunk}
#'   \item{elapsed_time}{Numeric; time taken in seconds}
#'   \item{success}{Logical; whether the operation succeeded}
#'   \item{rows_processed}{Integer; number of rows successfully processed}
#'   \item{throughput}{Numeric; requests processed per second}
#' }
#' @source Internal benchmarking of EndpointR functions
"batch_concurrent_benchmark"
