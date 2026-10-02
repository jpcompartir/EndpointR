# Tidy a TEI classification response with raw scores

With `raw_scores = TRUE`, TEI returns logits. This function applies the
softmax in R. TEI sorts each text's labels by score, so labels are
matched by name, never by position. A missing or NaN score gives `NA`
for that text.

## Usage

``` r
tidy_tei_classification_response(response)
```

## Arguments

- response:

  An httr2 response, or the parsed JSON: a list with one element per
  text, each a list of `{label, score}` objects

## Value

A tibble with one row per text and one column per label
