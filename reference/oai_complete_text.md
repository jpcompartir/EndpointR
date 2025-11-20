# Generate a completion for a single text using OpenAI's Chat Completions API

High-level function to generate a completion for a single text string.
This function handles the entire process from request creation to
response processing, with optional structured output support.

## Usage

``` r
oai_complete_text(
  text,
  model = "gpt-4.1-nano",
  system_prompt = NULL,
  schema = NULL,
  temperature = 0,
  max_tokens = 500L,
  key_name = "OPENAI_API_KEY",
  endpoint_url = "https://api.openai.com/v1/chat/completions",
  max_retries = 5L,
  timeout = 30,
  tidy = TRUE
)
```

## Arguments

- text:

  Character string to send to the model

- model:

  OpenAI model to use (default: "gpt-4.1-nano")

- system_prompt:

  Optional system prompt to guide the model's behaviour

- schema:

  Optional JSON schema for structured output (json_schema object or
  list)

- temperature:

  Sampling temperature (0-2), lower = more deterministic (default: 0)

- max_tokens:

  Maximum tokens in response (default: 500)

- key_name:

  Environment variable name for API key (default: "OPENAI_API_KEY")

- endpoint_url:

  OpenAI API endpoint URL

- max_retries:

  Maximum retry attempts for failed requests (default: 5)

- timeout:

  Request timeout in seconds (default: 30)

- tidy:

  Whether to attempt to parse structured output (default: TRUE)

## Value

Character string containing the model's response, or parsed JSON if
schema provided

## Examples

``` r
if (FALSE) { # \dontrun{
  # Simple completion
  response <- oai_complete_text(
    text = "Explain quantum computing in simple terms",
    temperature = 0.7
  )

  # With system prompt
  response <- oai_complete_text(
    text = "What are the main benefits?",
    system_prompt = "You are an expert in renewable energy",
    max_tokens = 200
  )

  # Structured output with schema
  sentiment_schema <- create_json_schema(
    name = "sentiment_analysis",
    schema = schema_object(
      sentiment = schema_string("positive, negative, or neutral"),
      confidence = schema_number("confidence score between 0 and 1"),
      required = list("sentiment", "confidence")
    )
  )

  result <- oai_complete_text(
    text = "I absolutely loved this product!",
    schema = sentiment_schema,
    temperature = 0
  )
} # }
```
