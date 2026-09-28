test_that("oai_file_upload errors when given inappropriate inputs", {
  expect_error(
    oai_file_upload("tmp"),
    "must be a file"
  )

  .tmp <- tempfile()
  writeLines("Hello!", .tmp)

  expect_error(
    oai_file_upload(.tmp, purpose = "life"),
    "should be one of"
  )
  
})

test_that("oai_file_upload no longer accepts the retired 'assistants' purpose", {
  .tmp <- tempfile()
  writeLines("Hello!", .tmp)

  expect_error(
    oai_file_upload(.tmp, purpose = "assistants"),
    "should be one of"
  )
})
