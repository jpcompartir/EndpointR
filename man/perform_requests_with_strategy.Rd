% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{perform_requests_with_strategy}
\alias{perform_requests_with_strategy}
\title{Perform multiple requests with configurable concurrency strategy}
\usage{
perform_requests_with_strategy(
  requests,
  concurrent_requests = 1,
  progress = TRUE
)
}
\arguments{
\item{requests}{List of httr2_request objects to perform}

\item{concurrent_requests}{Integer specifying maximum number of simultaneous requests (default: 1)}

\item{progress}{Logical indicating whether to show progress bar (default: TRUE)}
}
\value{
List of httr2_response objects or error objects for failed requests
}
\description{
Executes a list of HTTP requests either sequentially or in parallel.
Automatically chooses sequential processing when concurrent_requests = 1
or when there's only one request.
}
\details{
returns responses in the order that requests were sent, and returns errors in a predictable format.
}
\examples{
\dontrun{
  # Sequential processing
  responses <- perform_requests_with_strategy(
    requests = my_requests,
    concurrent_requests = 1
  )

  # Parallel processing with 5 concurrent requests
  responses <- perform_requests_with_strategy(
    requests = my_requests,
    concurrent_requests = 5,
    progress = TRUE
  )
}
}
