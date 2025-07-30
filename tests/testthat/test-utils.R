test_that("batch_vector takes a vector input and batches according to batch_size", {

  batch_texts <- c(
    "the",
    "input",
    "is",
    "irrelevant"
  )
  batched_texts <- expect_no_error(batch_vector(batch_texts, 2))

  expect_setequal(names(batched_texts), c("batch_indices", "batch_inputs"))

  expect_true(
    purrr::map_dbl(batched_texts, length) |>
      purrr::every(function(x) x == 2)
  )

  batched_3 <- expect_no_error(batch_vector(batch_texts, 3))
})


test_that(".handle_output_filename deals with auto, NULL, strings, and has input validation where needed",{

  expect_error(.handle_output_filename(x ="bosh", base_file_name = "oai_batch"),
               regexp = "must have a .csv")

  auto_file_name <- expect_no_error(.handle_output_filename(x ="auto", base_file_name = "oai_batch"))

  expect_true(grepl("oai_batch_.*csv$", auto_file_name))

  temp_file_name <- expect_no_error(.handle_output_filename(x = NULL))

  expect_true(grepl("batch_processing", temp_file_name))


  valid_file_name <-
    expect_no_error(.handle_output_filename("oai_batch.csv"))

  expect_true(identical(valid_file_name, "oai_batch.csv"))
})
