% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openai_embed.R
\name{oai_build_embedding_request}
\alias{oai_build_embedding_request}
\title{Build OpenAI embedding API request}
\usage{
oai_build_embedding_request(
  input,
  model = "text-embedding-3-small",
  dimensions = NULL,
  max_retries = 5,
  timeout = 20,
  endpoint_url = "https://api.openai.com/v1/embeddings",
  key_name = "OPENAI_API_KEY",
  verbose = FALSE
)
}
\arguments{
\item{input}{Character vector of text(s) to embed}

\item{model}{OpenAI embedding model to use (default: "text-embedding-3-small")}

\item{dimensions}{Number of embedding dimensions (NULL uses model default)}

\item{max_retries}{Maximum retry attempts for failed requests (default: 5)}

\item{timeout}{Request timeout in seconds (default: 20)}

\item{endpoint_url}{OpenAI API endpoint URL (default: OpenAI's embedding endpoint)}

\item{key_name}{Name of environment variable containing the API key (default: "OPENAI_API_KEY")}

\item{verbose}{Whether to enable verbose request logging (default: FALSE)}
}
\value{
An httr2 request object configured for the OpenAI embedding API.
The request object includes a \code{total_chars} attribute containing the
total character count of the input texts.
}
\description{
Creates an httr2 request object configured for OpenAI's embedding API.
This is a lower-level function that handles request configuration
including authentication, retries, and timeouts.
}
\details{
This function builds the HTTP request but does not execute it. The request
can then be performed using \code{httr2::req_perform()} or the package's
\code{hf_perform_request()} function.

Note that OpenAI has limits on input length - individual inputs cannot
exceed the model's token limit (typically 8192 tokens for embedding models).
Empty strings are not allowed as input.
}
\examples{
\dontrun{
  # Build a simple request
  req <- oai_build_embedding_request("Hello world")

  # Build request with custom dimensions
  req <- oai_build_embedding_request(
    input = "Hello world",
    dimensions = 512,
    model = "text-embedding-3-large"
  )

  # Perform the request
  response <- httr2::req_perform(req)
}
}
