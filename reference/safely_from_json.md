# Safely extract JSON

A wrapper around
[`jsonlite::fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
that returns a list instead of throwing an error when JSON parsing
fails. Uses
[`purrr::possibly`](https://purrr.tidyverse.org/reference/possibly.html)
to provide graceful error handling. To be explicit: you will not receive
an error message if your JSON failed to parse, just an empty list. This
property allows you to handle NULL/NA results in the way you'd like to.

## Usage

``` r
safely_from_json(...)
```

## Arguments

- ...:

  Arguments passed to
  [`jsonlite::fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
  The first argument should be `txt` (a JSON string, URL or file to
  parse). Additional arguments include: `simplifyVector` (coerce JSON
  arrays containing only primitives to atomic vectors, default TRUE),
  `simplifyDataFrame` (coerce JSON arrays containing objects to data
  frames, default TRUE), `simplifyMatrix` (coerce JSON arrays containing
  equal-length sub-arrays to matrices, default TRUE), and `flatten`
  (automatically flatten nested data frames, default FALSE).

## Value

Parsed JSON object on success, empty list on failure

## See also

[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html),
[`purrr::possibly()`](https://purrr.tidyverse.org/reference/possibly.html)

## Examples

``` r
# Valid JSON
safely_from_json('{"name": "John", "age": 30}')
#> $name
#> [1] "John"
#> 
#> $age
#> [1] 30
#> 

# Invalid JSON returns empty list instead of error
safely_from_json('{"invalid": json}')
#> list()

# Works with URLs and files too
safely_from_json("https://api.example.com/data.json")
#> Warning: URL 'https://api.example.com/data.json': status was 'Couldn't resolve host name'
#> list()
```
