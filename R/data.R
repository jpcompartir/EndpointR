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

#' Example embedding results from Hugging Face API
#'
#' A sample dataset containing text embeddings generated using Hugging Face's
#' embedding API. This dataset demonstrates the structure of results returned
#' by the `hf_embed_batch()` and `hf_embed_df()` functions.
#'
#' @format A data frame with 3 rows and 773 variables:
#' \describe{
#'   \item{id}{Integer; unique identifier for each text}
#'   \item{text}{Character; the original text that was embedded}
#'   \item{category}{Character; category classification of the text}
#'   \item{.error}{Logical; whether the embedding process failed}
#'   \item{.error_message}{Character; error message if embedding failed (NA if successful)}
#'   \item{V1, V2, ..., V768}{Numeric; embedding vector dimensions}
#' }
#' @source Generated using Hugging Face embedding model via EndpointR functions
"df_embeddings_hf"

#' Example sentiment classification results from Hugging Face API
#'
#' A sample dataset containing sentiment classification results generated using
#' Hugging Face's classification API. This dataset demonstrates the structure
#' of results returned by the `hf_classify_batch()` and `hf_classify_df()` functions.
#'
#' @format A data frame with 3 rows and 7 variables:
#' \describe{
#'   \item{id}{Integer; unique identifier for each text}
#'   \item{text}{Character; the original text that was classified}
#'   \item{category}{Character; category classification of the text}
#'   \item{NEGATIVE}{Numeric; probability score for negative sentiment (0-1)}
#'   \item{POSITIVE}{Numeric; probability score for positive sentiment (0-1)}
#'   \item{.error}{Logical; whether the classification process failed}
#'   \item{.error_message}{Character; error message if classification failed (NA if successful)}
#' }
#' @source Generated using Hugging Face sentiment classification model via EndpointR functions
"df_sentiment_classification_example"

#' Single sentiment classification result example
#'
#' A sample result from sentiment classification of a single text using
#' Hugging Face's classification API. This demonstrates the structure
#' returned by the `hf_classify_text()` function.
#'
#' @format A data frame with 1 row and 2 variables:
#' \describe{
#'   \item{NEGATIVE}{Numeric; probability score for negative sentiment (0-1)}
#'   \item{POSITIVE}{Numeric; probability score for positive sentiment (0-1)}
#' }
#' @source Generated using Hugging Face sentiment classification model via EndpointR functions
"sentiment_classification_example"

#' Single embedding result example from Hugging Face API
#'
#' A sample embedding vector result from processing a single text using
#' Hugging Face's embedding API. This demonstrates the structure returned
#' by the `hf_embed_text()` function.
#'
#' @format A data frame with 1 row and 768 variables:
#' \describe{
#'   \item{V1, V2, ..., V768}{Numeric; embedding vector dimensions representing
#'     the semantic encoding of the input text}
#' }
#' @source Generated using Hugging Face embedding model via EndpointR functions
"single_embedding_hf"
