# Create numeric property schema

Defines decimal number fields with optional range constraints. Use for
prices, percentages, ratings, or any continuous numeric data.

## Usage

``` r
schema_number(description = NULL, minimum = NULL, maximum = NULL)
```

## Arguments

- description:

  Human-readable field description

- minimum:

  Minimum allowed value (inclusive)

- maximum:

  Maximum allowed value (inclusive)

## Examples

``` r
# Product price
schema_number("Price in USD", minimum = 0)
#> $type
#> [1] "number"
#> 
#> $description
#> [1] "Price in USD"
#> 
#> $minimum
#> [1] 0
#> 

# Percentage score
schema_number("Confidence score", minimum = 0, maximum = 100)
#> $type
#> [1] "number"
#> 
#> $description
#> [1] "Confidence score"
#> 
#> $minimum
#> [1] 0
#> 
#> $maximum
#> [1] 100
#> 
```
