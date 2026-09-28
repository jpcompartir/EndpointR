test_that(".check_existing_output does nothing when directory does not exist", {
  non_existent <- file.path(tempdir(), "does_not_exist_12345")
  expect_no_error(.check_existing_output(non_existent, overwrite = FALSE))
})

test_that(".check_existing_output does nothing when directory exists but is empty", {
  empty_dir <- file.path(tempdir(), "empty_dir_test")
  dir.create(empty_dir, showWarnings = FALSE)
  on.exit(unlink(empty_dir, recursive = TRUE))

  expect_no_error(.check_existing_output(empty_dir, overwrite = FALSE))
})

test_that(".check_existing_output errors when directory contains parquet files and overwrite is FALSE", {
  existing_dir <- file.path(tempdir(), "existing_chunks_test")
  dir.create(existing_dir, showWarnings = FALSE)
  on.exit(unlink(existing_dir, recursive = TRUE))

  file.create(file.path(existing_dir, "chunk_001.parquet"))
  file.create(file.path(existing_dir, "chunk_002.parquet"))

  expect_error(
    .check_existing_output(existing_dir, overwrite = FALSE),
    regexp = "already contains 2 chunk file"
  )
})

test_that(".check_existing_output errors when directory contains metadata.json and overwrite is FALSE", {
  existing_dir <- file.path(tempdir(), "existing_meta_test")
  dir.create(existing_dir, showWarnings = FALSE)
  on.exit(unlink(existing_dir, recursive = TRUE))

  file.create(file.path(existing_dir, "metadata.json"))

  expect_error(
    .check_existing_output(existing_dir, overwrite = FALSE),
    regexp = "already contains"
  )
})

test_that(".check_existing_output allows overwrite when overwrite is TRUE", {
  existing_dir <- file.path(tempdir(), "overwrite_allowed_test")
  dir.create(existing_dir, showWarnings = FALSE)
  on.exit(unlink(existing_dir, recursive = TRUE))

  file.create(file.path(existing_dir, "chunk_001.parquet"))
  file.create(file.path(existing_dir, "chunk_002.parquet"))
  file.create(file.path(existing_dir, "metadata.json"))

  expect_no_error(suppressMessages(.check_existing_output(existing_dir, overwrite = TRUE)))
})

test_that(".check_existing_output with overwrite = TRUE deletes existing outputs so old and new chunks cannot mix", {
  existing_dir <- file.path(tempdir(), "overwrite_deletes_test")
  dir.create(existing_dir, showWarnings = FALSE)
  on.exit(unlink(existing_dir, recursive = TRUE))

  file.create(file.path(existing_dir, "chunk_001.parquet"))
  file.create(file.path(existing_dir, "chunk_002.parquet"))
  file.create(file.path(existing_dir, "metadata.json"))
  file.create(file.path(existing_dir, "notes.txt")) # unrelated file, must survive

  suppressMessages(.check_existing_output(existing_dir, overwrite = TRUE))

  expect_false(file.exists(file.path(existing_dir, "chunk_001.parquet")))
  expect_false(file.exists(file.path(existing_dir, "chunk_002.parquet")))
  expect_false(file.exists(file.path(existing_dir, "metadata.json")))
  expect_true(file.exists(file.path(existing_dir, "notes.txt")))
})

test_that(".check_existing_output error message does not mention metadata.json when only chunks exist", {
  chunks_only <- file.path(tempdir(), "msg_chunks_only_test")
  dir.create(chunks_only, showWarnings = FALSE)
  on.exit(unlink(chunks_only, recursive = TRUE))

  file.create(file.path(chunks_only, "chunk_001.parquet"))
  file.create(file.path(chunks_only, "chunk_002.parquet"))

  err <- expect_error(.check_existing_output(chunks_only, overwrite = FALSE))
  expect_false(grepl("metadata.json", conditionMessage(err), fixed = TRUE))
  expect_match(conditionMessage(err), "2 chunk files")
})

test_that(".check_existing_output error message mentions both chunks and metadata.json when both exist", {
  both_dir <- file.path(tempdir(), "msg_both_test")
  dir.create(both_dir, showWarnings = FALSE)
  on.exit(unlink(both_dir, recursive = TRUE))

  file.create(file.path(both_dir, "chunk_001.parquet"))
  file.create(file.path(both_dir, "metadata.json"))

  err <- expect_error(.check_existing_output(both_dir, overwrite = FALSE))
  expect_match(conditionMessage(err), "1 chunk file")
  expect_match(conditionMessage(err), "metadata.json", fixed = TRUE)
})

test_that(".check_existing_output error message does not report zero chunk files when only metadata.json exists", {
  meta_only <- file.path(tempdir(), "msg_meta_only_test")
  dir.create(meta_only, showWarnings = FALSE)
  on.exit(unlink(meta_only, recursive = TRUE))

  file.create(file.path(meta_only, "metadata.json"))

  err <- expect_error(.check_existing_output(meta_only, overwrite = FALSE))
  expect_false(grepl("0 chunk file", conditionMessage(err), fixed = TRUE))
  expect_match(conditionMessage(err), "metadata.json", fixed = TRUE)
})

test_that(".check_existing_output ignores non-parquet, non-metadata files", {
  dir_with_other <- file.path(tempdir(), "other_files_test")
  dir.create(dir_with_other, showWarnings = FALSE)
  on.exit(unlink(dir_with_other, recursive = TRUE))

  file.create(file.path(dir_with_other, "notes.txt"))
  file.create(file.path(dir_with_other, "script.R"))

  expect_no_error(.check_existing_output(dir_with_other, overwrite = FALSE))
})
