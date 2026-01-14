# Parse a Completions Batch Job into a Data Frame

Parses the JSONL content returned from a completed chat completions
batch job and converts it into a tidy data frame with one row per
response.

## Usage

``` r
oai_batch_parse_completions(content, original_df = NULL, id_var = NULL)
```

## Arguments

- content:

  Character string of JSONL content from the batch output file

- original_df:

  Optional original data frame to rename custom_id column

- id_var:

  If original_df provided, the column name to rename custom_id to

## Value

A tibble with custom_id (or renamed), content, .error, and .error_msg

## Examples

``` r
if (FALSE) { # \dontrun{
# After downloading batch results with oai_files_content()
content <- oai_files_content(status$output_file_id)
completions_df <- oai_batch_parse_completions(content)

# Optionally rename the ID column to match original data
completions_df <- oai_batch_parse_completions(
  content,
  original_df = my_df,
  id_var = query_id
)
} # }
```
