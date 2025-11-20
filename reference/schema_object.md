# Create JSON Schema object definitions

These helper functions simplify creating JSON Schema definitions for
structured outputs (e.g. OpenAI). They provide type-safe, validated
schemas that ensure consistent LLM responses matching your expected data
structure.

JSON Schema constrains LLM outputs to specific formats, preventing
parsing errors and ensuring reliable data extraction from unstructured
text. Create object schema with nested properties

Defines a JSON object with typed properties. Use this for structured
data like user profiles, API responses, or any nested data structure.
The LLM will return JSON matching exactly this schema.

## Usage

``` r
schema_object(..., required = NULL, additional_properties = FALSE)
```

## Arguments

- ...:

  Named arguments defining object properties (use other schema\_\*
  helpers)

- required:

  Character vector of required property names

- additional_properties:

  Whether to allow extra properties beyond those defined

## Examples

``` r
# User profile with required fields
schema_object(
  name = schema_string("Full name"),
  age = schema_integer("Age in years"),
  required = c("name", "age")
)
#> $type
#> [1] "object"
#> 
#> $properties
#> $properties$name
#> $properties$name$type
#> [1] "string"
#> 
#> $properties$name$description
#> [1] "Full name"
#> 
#> 
#> $properties$age
#> $properties$age$type
#> [1] "integer"
#> 
#> $properties$age$description
#> [1] "Age in years"
#> 
#> 
#> 
#> $additionalProperties
#> [1] FALSE
#> 
#> $required
#> [1] "name" "age" 
#> 
```
