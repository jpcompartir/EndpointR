# Create standardised error tibble for failed requests

Internal function to create a consistent error tibble structure. Ensures
uniform error reporting across different failure modes.

## Usage

``` r
.create_error_tibble(indices, error_message)
```

## Arguments

- indices:

  Vector of indices indicating original request positions

- error_message:

  Character string or condition object describing the error

## Value

A tibble with columns:

- original_index: Position in original request batch

- .error: Always TRUE for error tibbles

- .error_message: Character description of the error
