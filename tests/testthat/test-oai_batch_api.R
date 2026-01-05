test_that("oai_batch_build_embed_row creates a row of JSON and responds to its input arguments", {
  no_dims <- expect_no_error(
    oai_batch_build_embed_row(
      "hello",
      "1234"
    )
  )

  no_dims_str <- jsonlite::fromJSON(no_dims)

  with_dims <- expect_no_error(
    oai_batch_build_embed_row(
      "hello",
      "134",
      dimensions = 124
    )
  )

  with_dims_str <- jsonlite::fromJSON(with_dims)

  expect_equal(with_dims_str$body$dimensions, 124)
  expect_setequal(names(no_dims_str), names(with_dims_str))

  expect_true(no_dims_str$method == "POST")
  expect_equal(no_dims_str$url, "/v1/embeddings")
  expect_equal(no_dims_str$body$model, "text-embedding-3-small")
})
