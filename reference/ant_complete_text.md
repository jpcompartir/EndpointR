# Generate a completion for a single text using Anthropic's Messages API

High-level function to generate a completion for a single text string.
Handles request creation, execution, and response processing with
optional structured output support.

## Usage

``` r
ant_complete_text(
  text,
  model = .ANT_DEFAULT_MODEL,
  system_prompt = NULL,
  schema = NULL,
  temperature = 0,
  max_tokens = 500L,
  key_name = "ANTHROPIC_API_KEY",
  endpoint_url = .ANT_MESSAGES_ENDPOINT,
  max_retries = 5L,
  timeout = 30L,
  tidy = TRUE
)
```

## Arguments

- text:

  Character string to send to the model

- model:

  Anthropic model to use (default: "claude-sonnet-4-5-20250929")

- system_prompt:

  Optional system prompt

- schema:

  Optional JSON schema for structured output

- temperature:

  Sampling temperature (0-1)

- max_tokens:

  Maximum tokens in response

- key_name:

  Environment variable name for API key

- endpoint_url:

  Anthropic API endpoint URL

- max_retries:

  Maximum retry attempts

- timeout:

  Request timeout in seconds

- tidy:

  Whether to parse structured output (default: TRUE)

## Value

Character string with the model's response, or parsed JSON if schema
provided

## Examples

``` r
if (FALSE) { # \dontrun{
  # simple completion
  response <- ant_complete_text(
    text = "Explain quantum computing in simple terms",
    max_tokens = 500
  )

  # with structured output
  sentiment_schema <- create_json_schema(
    name = "sentiment",
    schema = schema_object(
      sentiment = schema_enum(c("positive", "negative", "neutral")),
      confidence = schema_number(minimum = 0, maximum = 1),
      required = c("sentiment", "confidence")
    )
  )
  result <- ant_complete_text(
    text = "I love this product!",
    schema = sentiment_schema
  )
} # }
```
