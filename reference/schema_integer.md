# Create integer property schema

Defines whole number fields with optional range constraints. Use for
counts, IDs, quantities, or discrete numeric values. More restrictive
than schema_number.

## Usage

``` r
schema_integer(description = NULL, minimum = NULL, maximum = NULL)
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
# Item quantity
schema_integer("Quantity ordered", minimum = 1)
#> $type
#> [1] "integer"
#> 
#> $description
#> [1] "Quantity ordered"
#> 
#> $minimum
#> [1] 1
#> 

# Rating scale
schema_integer("Star rating", minimum = 1, maximum = 5)
#> $type
#> [1] "integer"
#> 
#> $description
#> [1] "Star rating"
#> 
#> $minimum
#> [1] 1
#> 
#> $maximum
#> [1] 5
#> 
```
