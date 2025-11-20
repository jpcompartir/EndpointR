# Create enumerated property schema

Defines fields constrained to specific allowed values. More flexible
than schema_string with enum parameter - supports numeric enums and
mixed types. Use for categories, status codes, or any multi-choice
field.

## Usage

``` r
schema_enum(values, description = NULL, type = "string")
```

## Arguments

- values:

  Vector of allowed values

- description:

  Human-readable field description

- type:

  Data type of enum values ("string", "integer", or "number")

## Examples

``` r
# Status categories
schema_enum(c("draft", "published", "archived"), "Document status")
#> $type
#> [1] "string"
#> 
#> $enum
#> [1] "draft"     "published" "archived" 
#> 
#> $description
#> [1] "Document status"
#> 

# Priority levels as numbers
schema_enum(c(1, 2, 3, 4, 5), "Priority level", type = "integer")
#> $type
#> [1] "integer"
#> 
#> $enum
#> [1] 1 2 3 4 5
#> 
#> $description
#> [1] "Priority level"
#> 
```
