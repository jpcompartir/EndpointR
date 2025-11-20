# Retrieve an API key which has been stored as an Environment Variable.

Retrieve an API key from .Renviron. The API key should have been set via
[`set_api_key()`](https://jpcompartir.github.io/EndpointR/reference/set_api_key.md)or
manually placed in your .Renviron file.

## Usage

``` r
get_api_key(key_name)
```

## Arguments

- key_name:

  The name of the API in the format "ENDPOINT_API_KEY" -\>
  "ANTHROPIC_API_KEY"

## Value

character

## Examples

``` r
if (FALSE) { # \dontrun{
  # retrieve an Anthropic API key
  anthropic_key <- get_api_key("ANTHROPIC_API_KEY")

  # use the key in a function call
  my_function(api_key = anthropic_key)
} # }
```
