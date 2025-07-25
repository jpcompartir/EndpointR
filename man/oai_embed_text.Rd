% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openai_embed.R
\name{oai_embed_text}
\alias{oai_embed_text}
\title{Generate embeddings for a single text using OpenAI}
\usage{
oai_embed_text(
  text,
  model = "text-embedding-3-small",
  dimensions = NULL,
  max_retries = 5,
  timeout = 20,
  endpoint_url = "https://api.openai.com/v1/embeddings",
  key_name = "OPENAI_API_KEY",
  tidy = TRUE
)
}
\arguments{
\item{text}{Character string to generate embeddings for (must be non-empty)}

\item{model}{OpenAI embedding model to use (default: "text-embedding-3-small")}

\item{dimensions}{Number of embedding dimensions (NULL uses model default)}

\item{max_retries}{Maximum retry attempts for failed requests (default: 5)}

\item{timeout}{Request timeout in seconds (default: 20)}

\item{endpoint_url}{OpenAI API endpoint URL (default: OpenAI's embedding endpoint)}

\item{key_name}{Name of environment variable containing the API key (default: "OPENAI_API_KEY")}

\item{tidy}{Whether to return a tidy tibble format (default: TRUE)}
}
\value{
If \code{tidy = TRUE}, returns a tibble with embedding vectors as columns
(V1, V2, etc.). If \code{tidy = FALSE}, returns the raw httr2 response object.
}
\description{
High-level function to generate embeddings for a single text string using
OpenAI's embedding API. This function handles the entire process from request
creation to response processing.
}
\details{
This function is designed for single text inputs. For processing multiple
texts, use \code{oai_embed_batch()} which is more efficient for batch operations.

The function automatically handles API authentication, request retries,
and error handling. By default, it returns a tidy tibble with embedding
vectors as columns, but you can get the raw response by setting \code{tidy = FALSE}.
}
\examples{
\dontrun{
  # Generate embeddings for a single text
  embeddings <- oai_embed_text("Hello world")

  # Use a different model with custom dimensions
  embeddings <- oai_embed_text(
    text = "Hello world",
    model = "text-embedding-3-large",
    dimensions = 1024
  )

  # Get raw response instead of tidy format
  raw_response <- oai_embed_text(
    text = "Hello world",
    tidy = FALSE
  )
}
}
