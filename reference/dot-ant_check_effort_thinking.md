# Stop when `effort` and `thinking` can't work on `model`

The API checks requests in a batch only after the whole batch has run,
so we check the rules we know about before sending. Models that match
none of the rule regexes are not checked.

## Usage

``` r
.ant_check_effort_thinking(
  model,
  effort,
  thinking_config,
  max_tokens,
  call = rlang::caller_env()
)
```
