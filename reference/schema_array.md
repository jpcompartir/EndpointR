# Create array property schema

Defines list/array fields containing multiple items of the same type.
Use for tags, categories, or any collection data. Can constrain array
length.

## Usage

``` r
schema_array(items, description = NULL, min_items = NULL, max_items = NULL)
```

## Arguments

- items:

  Schema definition for array elements (use other schema\_\* helpers)

- description:

  Human-readable field description

- min_items:

  Minimum number of array elements

- max_items:

  Maximum number of array elements

## Examples

``` r
# List of tags
schema_array(schema_string(), "Product tags", max_items = 10)
#> $type
#> [1] "array"
#> 
#> $items
#> $items$type
#> [1] "string"
#> 
#> 
#> $description
#> [1] "Product tags"
#> 
#> $maxItems
#> [1] 10
#> 

# Array of objects
schema_array(
  schema_object(
    name = schema_string("Ingredient name"),
    amount = schema_number("Amount needed")
  ),
  "Recipe ingredients"
)
#> $type
#> [1] "array"
#> 
#> $items
#> $items$type
#> [1] "object"
#> 
#> $items$properties
#> $items$properties$name
#> $items$properties$name$type
#> [1] "string"
#> 
#> $items$properties$name$description
#> [1] "Ingredient name"
#> 
#> 
#> $items$properties$amount
#> $items$properties$amount$type
#> [1] "number"
#> 
#> $items$properties$amount$description
#> [1] "Amount needed"
#> 
#> 
#> 
#> $items$additionalProperties
#> [1] FALSE
#> 
#> 
#> $description
#> [1] "Recipe ingredients"
#> 
```
