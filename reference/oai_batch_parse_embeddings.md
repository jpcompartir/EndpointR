# Parse an Embeddings Batch Job into a Data Frame

Parses the JSONL content returned from a completed embeddings batch job
and converts it into a tidy data frame with one row per embedding.

## Usage

``` r
oai_batch_parse_embeddings(content, original_df = NULL, id_var = NULL)
```

## Arguments

- content:

  Character string of JSONL content from the batch output file

- original_df:

  Optional original data frame to rename custom_id column

- id_var:

  If original_df provided, the column name to rename custom_id to

## Value

A tibble with custom_id (or renamed), .error, .error_msg, and embedding
dimensions (V1, V2, ..., Vn)

## Examples

``` r
if (FALSE) { # \dontrun{
# After downloading batch results with oai_files_content()
content <- oai_files_content(status$output_file_id)
embeddings_df <- oai_batch_parse_embeddings(content)

# Optionally rename the ID column to match original data
embeddings_df <- oai_batch_parse_embeddings(
  content,
  original_df = my_df,
  id_var = doc_id
)
} # }
```
