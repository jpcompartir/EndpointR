test_that("oai_file_upload errors when given inappropriate inputs", {
  expect_error(
    oai_file_upload("tmp"),
    "must be a file"
  )

  .tmp <- tempfile()
  writeLines(.tmp, "Hello!")

  expect_error(
    oai_file_upload(.tmp, purpose = "life"),
    "should be one of"
  )
  
})
