test_that("oai_batch_build_embed_req creates a row of JSON and responds to its input arguments", {
  no_dims <- expect_no_error(
    oai_batch_build_embed_req(
      "hello",
      "1234"
    )
  )

  no_dims_str <- jsonlite::fromJSON(no_dims)

  with_dims <- expect_no_error(
    oai_batch_build_embed_req(
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

test_that("oai_batch_prepare_completions creates valid JSONL", {
  test_df <- tibble::tibble(
    id = c("a", "b"),
    text = c("Hello", "World")
  )

  result <- oai_batch_prepare_completions(
    df = test_df,
    text_var = text,
    id_var = id
  )

  lines <- strsplit(result, "\n")[[1]]
  expect_equal(length(lines), 2)

  parsed <- purrr::map(lines, \(x) jsonlite::fromJSON(x, simplifyVector = FALSE))
  expect_equal(parsed[[1]]$custom_id, "a")
  expect_equal(parsed[[2]]$custom_id, "b")
  expect_equal(parsed[[1]]$body$messages[[1]]$content, "Hello")
})
test_that("oai_batch_parse_embeddings handles success response", {
  mock_content <- '{"custom_id":"1","response":{"body":{"data":[{"embedding":[0.1,0.2,0.3]}]}},"error":null}'

  result <- oai_batch_parse_embeddings(mock_content)

  expect_equal(nrow(result), 1)
  expect_equal(result$custom_id, "1")
  expect_false(result$.error)
  expect_true("V1" %in% names(result))
  expect_equal(result$V1, 0.1)
  expect_equal(result$V2, 0.2)
  expect_equal(result$V3, 0.3)
})
test_that("oai_batch_parse_embeddings handles error response", {
  mock_content <- '{"custom_id":"1","response":null,"error":{"message":"Rate limit exceeded"}}'

  result <- oai_batch_parse_embeddings(mock_content)

  expect_equal(nrow(result), 1)
  expect_true(result$.error)
  expect_equal(result$.error_msg, "Rate limit exceeded")
})

test_that("oai_batch_parse_embeddings handles multiple rows", {
  mock_content <- paste0(
    '{"custom_id":"1","response":{"body":{"data":[{"embedding":[0.1,0.2]}]}},"error":null}',
    '\n',
    '{"custom_id":"2","response":{"body":{"data":[{"embedding":[0.3,0.4]}]}},"error":null}'
  )

  result <- oai_batch_parse_embeddings(mock_content)

  expect_equal(nrow(result), 2)
  expect_equal(result$custom_id, c("1", "2"))
  expect_equal(result$V1, c(0.1, 0.3))
})
