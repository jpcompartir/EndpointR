test_that("validate_hf_endpoint errors if given invalid URL", {

  key_name = "TEST_API_KEY"

  withr::with_envvar(
    c("TEST_API_KEY" = "gibberish"),{
    expect_error(
      validate_hf_endpoint(
        endpoint_url = "xxx_yy.com",
        key_name = key_name
      ),
      regexp = "Cannot connect to Hugging")
  })

  # testing strategy is a bit difficult as we're working with endpoints that won't be up etc. so we don't *really* want to do check for success, at least not when CI/CD runs. Need to think more on this, they may just be local tests for most things - which is not ideal.

})

# following on from comment in 'validate_hf_endpoint' test, tidy functions are checking for httr2 response, so we need to mimic a httr2 response that returns embeddings. Did this in the start of package development with webfakes, but httr2 actually has objects for this - `response`, `response_json` and some mocking objects, so we can keep it quite simple.

test_that("tidy_embedding_response tidies single requests and batches", {
  single_resp <- httr2::response_json(
    status_code = 200,
    headers = list("content-type" = "application/json"),
    body = list(c(0.01, 0.02, 0.03, 0.04, 0.05))
    )

  tidied_resp <- expect_no_error(tidy_embedding_response(single_resp))
  expect_equal(nrow(tidied_resp), 1)
  expect_equal(ncol(tidied_resp), 5)

  batch_resp <- httr2::response_json(
    status_code = 200,
    headers = list("content-type" = "application/json"),
    body = list(
      c(0.1, 0.2, 0.3, 0.4, 0.5),
      c(0.6, 0.7, 0.8, 0.9, 1.0),
      c(1.1, 1.2, 1.3, 1.4, 1.5)
    )
  )

  tidied_batch_resp <- expect_no_error(tidy_embedding_response(batch_resp))
  expect_equal(nrow(tidied_batch_resp), 3)
  expect_equal(ncol(tidied_batch_resp), 5)
})



test_that("hf_embed_batch works correctly with tidy_func parameter added", {

  result <- expect_no_error(
    hf_embed_batch(
      texts = c("text1", "text2"),
      endpoint_url = server$url("/test_batch_embedding"),
      key_name = "HF_TEST_API_KEY",
      batch_size = 2,
      include_texts = FALSE,
      relocate_col = 1)
    )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(c("V1", "V2", "V3", ".error", ".error_message") %in% names(result)))
})

test_that("hf_embed_batch allows custom tidy_func", {
  custom_tidy <- function(response) {
    result <- tidy_embedding_response(response)
    result$custom_col <- "custom_value"
    result
  }

  result <- hf_embed_batch(
    texts = "test",
    endpoint_url = server$url("/test_batch_embedding"),
    key_name = "HF_TEST_API_KEY",
    tidy_func = custom_tidy,
    include_texts = FALSE,
    relocate_col = 1

  )

  expect_true("custom_col" %in% names(result))
  expect_equal(result$custom_col, c("custom_value", "custom_value"))
})

test_that("hf_embed_chunks replaces hf_embed_batch", {
  texts <- paste0("text", 1:6)
  ids <- paste0('id', 1:length(texts))
  temp_dir <- withr::local_tempdir()
  expected_cols <- c("id", ".error", ".error_msg", ".chunk", "V1", "V2", "V3")


  chunk_2 <- expect_no_error(hf_embed_chunks(
    texts = texts,
    ids = ids,
    endpoint_url = server$url("/test_embedding"), # use this as chunks are 1:1 request-row-response, not batches. I.e. don't need a chunk endpoint.=
    key_name = "HF_TEST_API_KEY",
    chunk_size = 2,
    concurrent_requests =1,
    output_dir = temp_dir
  )) |> suppressMessages()

  expect_setequal(unique(chunk_2$`.chunk`), c(1, 2, 3))
  expect_setequal(names(chunk_2), expected_cols)

  chunk_1 <- expect_no_error(hf_embed_chunks(
    texts = texts,
    ids = ids,
    endpoint_url = server$url("/test_embedding"), # use this as chunks are 1:1 request-row-response, not batches. I.e. don't need a chunk endpoint.=
    key_name = "HF_TEST_API_KEY",
    chunk_size = 1,
    concurrent_requests =1,
    output_dir = temp_dir
  )) |> suppressMessages()

  expect_setequal(unique(chunk_1$`.chunk`), 1:6)

})

test_that("hf_embed_df works correctly with real endpoint", {
  test_df <- data.frame(
    id = paste0("id", 1:2),
    text = c("text1", "text2"),
    stringsAsFactors = FALSE
  )
  output_dir <- withr::local_tempdir()

  result <- expect_no_error(
    hf_embed_df(
      df = test_df,
      text_var = text,
      id_var = id,
      endpoint_url = server$url("/test_embedding"),
      key_name = "HF_TEST_API_KEY",
      chunk_size = 1,
      output_dir = output_dir
    )
  ) |>
    suppressMessages()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("id", "V1", "V2", "V3", ".error", ".error_msg", ".chunk") %in% names(result)))
  expect_equal(result$id, c("id1", "id2"))
  expect_equal(result$V1, c(0.1, 0.1), tolerance = 1e-7)
  expect_equal(result$V2, c(0.2, 0.2), tolerance = 1e-7)
  expect_equal(result$V3, c(0.3, 0.3), tolerance = 1e-7)
  expect_equal(result$.error, c(FALSE, FALSE))
})

test_that("hf_embed_df works with different batch sizes", {
  test_df <- data.frame(
    id = c(paste0("id", 1:2)),
    text = c("text1", "text2"),
    stringsAsFactors = FALSE
  )
  temp_dir <- withr::local_tempdir()

  result <- expect_no_error(
    hf_embed_df(
      df = test_df,
      text_var = text,
      id_var = id,
      endpoint_url = server$url("/test_embedding"),
      key_name = "HF_TEST_API_KEY",
      chunk_size = 1,
      concurrent_requests = 1,
      output_dir = output_dir
    )
  ) |>
    suppressMessages()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("id", ".chunk", ".error", ".error_msg") %in% names(result)))
  expect_equal(result$.error, c(FALSE, FALSE))
})
