# Build an OpenAI API Chat Completions request

This function constructs a httr2 request object specifically tailored
for interacting with OpenAI's Chat Completions API. It handles the
formatting of messages, model parameters, and optional JSON schema for
structured responses.

## Usage

``` r
oai_build_completions_request(
  input,
  endpointr_id = NULL,
  model = "gpt-4.1-nano",
  temperature = 0,
  max_tokens = 500L,
  schema = NULL,
  system_prompt = NULL,
  key_name = "OPENAI_API_KEY",
  endpoint_url = "https://api.openai.com/v1/chat/completions",
  timeout = 20,
  max_retries = 5
)
```

## Arguments

- input:

  Text input to send to the model

- endpointr_id:

  An id that will persist through to response

- model:

  OpenAI model to use (default: "gpt-4.1-nano")

- temperature:

  Sampling temperature (0-2), higher values = more randomness

- max_tokens:

  Maximum tokens in response

- schema:

  Optional JSON schema for structured output (json_schema object or
  list)

- system_prompt:

  Optional system prompt

- key_name:

  Environment variable name for API key

- endpoint_url:

  OpenAI API endpoint URL

- timeout:

  Request timeout in seconds

- max_retries:

  Maximum number of retry attempts for failed requests

## Value

An httr2 request object

## Details

This function simplifies the process of making calls to the OpenAI Chat
Completions API by assembling the request body according to the API's
specifications.

The `input` and `system_prompt` (if provided) are automatically
structured into the required 'messages' array format for the API.

For structured outputs (JSON mode), you will need to provide a valid
JSON schema via the `schema` parameter. This can be a pre-formatted list
or an object of class "json_schema". If a schema is provided, the
function will automatically set `schema$additionalProperties <- FALSE`
and ensure `schema$strict <- TRUE` (if `strict` is not already defined
in the schema) to encourage more predictable and reliable structured
outputs from the API. It's also a good idea to set temperature to 0 when
extracting structured outputs.

## See also

[Completions vs Responses
API](https://platform.openai.com/docs/guides/responses-vs-chat-completions)
