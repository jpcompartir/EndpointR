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
