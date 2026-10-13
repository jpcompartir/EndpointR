# Pull the result fields out of a parsed Anthropic message

Used for Messages responses and for the `message` in a succeeded batch
result, which have the same shape, so both routes return the same
columns.

## Usage

``` r
.ant_message_fields(msg)
```

## Arguments

- msg:

  A parsed message, e.g. from
  [`httr2::resp_body_json()`](https://httr2.r-lib.org/reference/resp_body_raw.html)

## Value

A list of `content`, `stop_reason`, `input_tokens`, `output_tokens` and
`thinking_tokens`
