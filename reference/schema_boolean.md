# Create boolean property schema

Defines true/false fields. Use for flags, yes/no questions, or binary
states. Ensures LLM returns exactly true or false, not "yes"/"no"
strings.

## Usage

``` r
schema_boolean(description = NULL)
```

## Arguments

- description:

  Human-readable field description

## Examples

``` r
# Feature flag
schema_boolean("Has premium subscription")
#> $type
#> [1] "boolean"
#> 
#> $description
#> [1] "Has premium subscription"
#> 

# Classification result
schema_boolean("Contains sensitive information")
#> $type
#> [1] "boolean"
#> 
#> $description
#> [1] "Contains sensitive information"
#> 
```
