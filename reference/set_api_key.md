# Set your API keys so they can be accessed by EndpointR

Set an API key for each endpoint - where endpoint could be Anthropic,
OpenAI, a specific Hugging Face Inference endpoint, or another supported
provider. Add overwrite=TRUE if you need to update an existing key. You
will then be able to retrieve the key with the
[`get_api_key()`](https://jpcompartir.github.io/EndpointR/reference/get_api_key.md)
function.

## Usage

``` r
set_api_key(key_name, overwrite = FALSE)
```

## Arguments

- key_name:

  The name of the API in the format "ENDPOINT_API_KEY" -\>
  "ANTHROPIC_API_KEY"

- overwrite:

  Whether to overwrite an existing value for the API key.

## Value

Nothing

## Examples

``` r
if (FALSE) { # \dontrun{
  # set an Anthropic API key
  set_api_key("ANTHROPIC_API_KEY")

  # update an existing OpenAI key
  set_api_key("OPENAI_API_KEY", overwrite = TRUE)
} # }
```
