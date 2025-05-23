% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hf_inference.R
\name{hf_build_request_df}
\alias{hf_build_request_df}
\title{Prepare embedding requests for texts in a data frame}
\usage{
hf_build_request_df(
  df,
  text_var,
  id_var,
  endpoint_url,
  key_name,
  parameters = list(),
  max_retries = 3,
  timeout = 10,
  validate = FALSE
)
}
\arguments{
\item{df}{A data frame containing texts to embed}

\item{text_var}{Name of the column containing text to send to the endpoint}

\item{id_var}{Name of the column to use as ID (optional)}

\item{endpoint_url}{The URL of the Hugging Face Inference API endpoint}

\item{key_name}{Name of the environment variable containing the API key}

\item{parameters}{Parameters to send with inputs}

\item{max_retries}{Maximum number of retry attempts for failed requests}

\item{timeout}{Request timeout in seconds}

\item{validate}{Whether to validate the endpoint before creating requests}
}
\value{
A data frame with the original data plus request objects
}
\description{
Creates httr2 request objects for each text in a data frame column.
Thus function handles request creation, it does not handle performing the request,
or tidying the response. To perform the request, select the appropriate
\verb{*_perform_*} function.
}
\examples{
\dontrun{
  # Prepare requests for a data frame
  df <- data.frame(
    id = 1:3,
    text = c("First example", "Second example", "Third example")
  )

  requests_df <- hf_build_request_df(
    df = df,
    text_var = text,
    endpoint_url = "https://my-endpoint.huggingface.cloud",
    id_var = id
  )
}
}
