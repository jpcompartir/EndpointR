# Create JSON Schema S7 class for structured outputs

S7 class for JSON Schema definitions used in, e.g. OpenAI structured
outputs

## Usage

``` r
json_schema(
  name = character(0),
  schema = list(),
  strict = TRUE,
  description = ""
)
```

## Arguments

- name:

  Name of the schema

- schema:

  The JSON schema definition as a list

- strict:

  Whether to enforce strict mode (default TRUE)

- description:

  Optional description of the schema
