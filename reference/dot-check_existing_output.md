# Check for existing output files in a directory

Check for existing output files in a directory

## Usage

``` r
.check_existing_output(output_dir, overwrite = FALSE)
```

## Arguments

- output_dir:

  Path to the output directory.

- overwrite:

  If `FALSE` (default), errors when the directory already contains
  `.parquet` or `metadata.json` files. If `TRUE`, deletes those files
  before returning, so the directory only ever holds one run's outputs -
  stale chunks from a previous run cannot mix with new ones. Other files
  in the directory are left untouched.
