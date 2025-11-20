# Example sentiment classification results from Hugging Face API

A sample dataset containing sentiment classification results generated
using Hugging Face's classification API. This dataset demonstrates the
structure of results returned by the
[`hf_classify_batch()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_batch.md)
and
[`hf_classify_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_df.md)
functions.

## Usage

``` r
df_sentiment_classification_example
```

## Format

A data frame with 3 rows and 7 variables:

- id:

  Integer; unique identifier for each text

- text:

  Character; the original text that was classified

- category:

  Character; category classification of the text

- NEGATIVE:

  Numeric; probability score for negative sentiment (0-1)

- POSITIVE:

  Numeric; probability score for positive sentiment (0-1)

- .error:

  Logical; whether the classification process failed

- .error_message:

  Character; error message if classification failed (NA if successful)

## Source

Generated using Hugging Face sentiment classification model via
EndpointR functions
