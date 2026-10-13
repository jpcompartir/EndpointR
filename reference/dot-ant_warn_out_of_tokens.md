# Warn when responses ran out of tokens before writing any text

Thinking tokens count towards `max_tokens`, so a model can spend all of
them thinking and return no text.

## Usage

``` r
.ant_warn_out_of_tokens(content, stop_reason)
```
