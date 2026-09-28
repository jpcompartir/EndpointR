# These tests verify that _chunks functions refuse to overwrite existing output
# by default, and allow it when overwrite = TRUE.
# They test the parameter wiring, not the API calls themselves.

test_that("ant_complete_chunks errors when output_dir contains existing chunks", {
  existing_dir <- file.path(tempdir(), "ant_overwrite_test")
  dir.create(existing_dir, showWarnings = FALSE)
  on.exit(unlink(existing_dir, recursive = TRUE))
  file.create(file.path(existing_dir, "chunk_001.parquet"))

  expect_error(
    ant_complete_chunks(
      texts = c("a", "b"),
      ids = c(1, 2),
      output_dir = existing_dir
    ),
    regexp = "already contains"
  )
})

test_that("ant_complete_df passes overwrite protection through to ant_complete_chunks", {
  existing_dir <- file.path(tempdir(), "ant_df_overwrite_test")
  dir.create(existing_dir, showWarnings = FALSE)
  on.exit(unlink(existing_dir, recursive = TRUE))
  file.create(file.path(existing_dir, "chunk_001.parquet"))

  df <- tibble::tibble(id = 1:2, text = c("a", "b"))

  expect_error(
    ant_complete_df(
      df = df,
      text_var = text,
      id_var = id,
      output_dir = existing_dir
    ),
    regexp = "already contains"
  )
})

test_that("oai_complete_df passes overwrite protection through to oai_complete_chunks", {
  existing_dir <- file.path(tempdir(), "oai_df_overwrite_test")
  dir.create(existing_dir, showWarnings = FALSE)
  on.exit(unlink(existing_dir, recursive = TRUE))
  file.create(file.path(existing_dir, "chunk_001.parquet"))

  df <- tibble::tibble(id = 1:2, text = c("a", "b"))

  expect_error(
    oai_complete_df(
      df = df,
      text_var = text,
      id_var = id,
      output_dir = existing_dir
    ),
    regexp = "already contains"
  )
})

test_that("oai_complete_chunks errors when output_dir contains existing chunks", {
  existing_dir <- file.path(tempdir(), "oai_comp_overwrite_test")
  dir.create(existing_dir, showWarnings = FALSE)
  on.exit(unlink(existing_dir, recursive = TRUE))
  file.create(file.path(existing_dir, "chunk_001.parquet"))

  expect_error(
    oai_complete_chunks(
      texts = c("a", "b"),
      ids = c(1, 2),
      output_dir = existing_dir
    ),
    regexp = "already contains"
  )
})

test_that("oai_embed_chunks errors when output_dir contains existing chunks", {
  existing_dir <- file.path(tempdir(), "oai_embed_overwrite_test")
  dir.create(existing_dir, showWarnings = FALSE)
  on.exit(unlink(existing_dir, recursive = TRUE))
  file.create(file.path(existing_dir, "chunk_001.parquet"))

  expect_error(
    oai_embed_chunks(
      texts = c("a", "b"),
      ids = c(1, 2),
      output_dir = existing_dir
    ),
    regexp = "already contains"
  )
})

test_that("hf_embed_chunks errors when output_dir contains existing chunks", {
  existing_dir <- file.path(tempdir(), "hf_embed_overwrite_test")
  dir.create(existing_dir, showWarnings = FALSE)
  on.exit(unlink(existing_dir, recursive = TRUE))
  file.create(file.path(existing_dir, "chunk_001.parquet"))

  expect_error(
    hf_embed_chunks(
      texts = c("a", "b"),
      ids = c(1, 2),
      endpoint_url = "https://fake-endpoint.com",
      output_dir = existing_dir
    ),
    regexp = "already contains"
  )
})

test_that("hf_classify_chunks errors when output_dir contains existing chunks", {
  existing_dir <- file.path(tempdir(), "hf_classify_overwrite_test")
  dir.create(existing_dir, showWarnings = FALSE)
  on.exit(unlink(existing_dir, recursive = TRUE))
  file.create(file.path(existing_dir, "chunk_001.parquet"))

  expect_error(
    hf_classify_chunks(
      texts = c("a", "b", "c"),
      ids = c(1, 2, 3),
      endpoint_url = "https://fake-endpoint.com",
      output_dir = existing_dir
    ),
    regexp = "already contains"
  )
})
