# Split a data frame into chunks for batch processing

Splits a data frame into chunks of specified size.

## Usage

``` r
chunk_dataframe(df, chunk_size)
```

## Arguments

- df:

  A data frame to split into batches

- chunk_size:

  Number of rows per batch

## Value

A list of data frames, each with at most chunk_size rows
