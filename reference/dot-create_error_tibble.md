# Create standardised error tibble for failed requests

Internal function to create a consistent error tibble structure. Ensures
uniform error reporting across different failure modes.

## Usage

``` r
.create_error_tibble(indices, error_msg, status = NA_integer_)
```

## Arguments

- indices:

  Vector of indices indicating original request positions

- error_msg:

  Character string or condition object describing the error

- status:

  HTTP status code (integer) or NA_integer\_ for non-HTTP errors.
  Defaults to NA_integer\_.

## Value

A tibble with columns:

- original_index: Position in original request batch

- .error: TRUE for errors

- .error_msg: Character description of the error

- .status: HTTP status code (integer) or NA for non-HTTP errors
