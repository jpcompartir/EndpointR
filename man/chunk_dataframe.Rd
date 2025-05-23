% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{chunk_dataframe}
\alias{chunk_dataframe}
\title{Split a data frame into chunks for batch processing}
\usage{
chunk_dataframe(df, chunk_size)
}
\arguments{
\item{df}{A data frame to split into batches}

\item{chunk_size}{Number of rows per batch}
}
\value{
A list of data frames, each with at most chunk_size rows
}
\description{
Splits a data frame into chunks of specified size.
}
\keyword{internal}
