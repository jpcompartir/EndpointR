# Single sentiment classification result example

A sample result from sentiment classification of a single text using
Hugging Face's classification API. This demonstrates the structure
returned by the
[`hf_classify_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_text.md)
function.

## Usage

``` r
sentiment_classification_example
```

## Format

A data frame with 1 row and 2 variables:

- NEGATIVE:

  Numeric; probability score for negative sentiment (0-1)

- POSITIVE:

  Numeric; probability score for positive sentiment (0-1)

## Source

Generated using Hugging Face sentiment classification model via
EndpointR functions
