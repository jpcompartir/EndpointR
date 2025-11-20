# Create string property schema

Defines text fields with optional constraints. Use for names,
descriptions, or any textual data. Can restrict to specific values via
enum parameter.

## Usage

``` r
schema_string(description = NULL, enum = NULL)
```

## Arguments

- description:

  Human-readable field description (helps LLM understand context)

- enum:

  Character vector of allowed values (creates dropdown-like constraint)

## Examples

``` r
# Simple text field
schema_string("User's email address")
#> $type
#> [1] "string"
#> 
#> $description
#> [1] "User's email address"
#> 

# Constrained to specific values
schema_string("Sentiment", enum = c("positive", "negative", "neutral"))
#> $type
#> [1] "string"
#> 
#> $description
#> [1] "Sentiment"
#> 
#> $enum
#> [1] "positive" "negative" "neutral" 
#> 
```
