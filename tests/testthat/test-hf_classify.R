test_that("tidy_classification_response handles a single response with $labels and $score in the body", {

  sentiment_scores <- list(
    list(
      list(label = "positive", score = 0.01),
      list(label = "negative", score = 0.98),
      list(label = "neutral", score = 0.01)
      )
    )

  tidied_response <- expect_no_error(
    httr2::response_json(
      status_code = 200L,
      body = sentiment_scores
    ) |>
      httr2::resp_body_json() |>
      tidy_classification_response()
  )

  expect_setequal(names(tidied_response), c("positive", "negative", "neutral"))


  spam_scores <- list(
    list(
      list(label = "spam", score = 0.55),
      list(label = "not_spam", score = 0.45)
    )
  )

  tidied_spam_response <- expect_no_error(
    httr2::response_json(
      status_code = 200L,
      body = spam_scores,
    ) |>
      httr2::resp_body_json() |>
      tidy_classification_response()
  )

  expect_equal(nrow(tidied_spam_response), 1)
  expect_setequal(names(tidied_spam_response), c("spam", "not_spam"))
})

test_that("tidy_batch_classification_response handes a batch response with multiple $label and multiple $score in the body",{

  batch_spam_scores <- list(
    list(
      list(label = "spam", score = 0.58),
      list(label = "not_spam", score = 0.42)
    ),
    list(
      list(label = "spam", score = 0.58),
      list(label = "not_spam", score = 0.42)
    )
  )

  # single batch case
  mock_batch_response <- httr2::response_json(
    body = batch_spam_scores,
    status_code = 200L,
    headers = list("Content-Type" = "application/json")
  )

  tidied_batch_response <- expect_no_error(tidy_batch_classification_response(mock_batch_response))

  expect_setequal(names(tidied_batch_response), c("spam", "not_spam"))
  expect_equal(nrow(tidied_batch_response), 2)

})


test_that("hf_classify_text takes a text and returns a tidied classification response", {

  # test uses the webfake app set up in helper-webfake.R

  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  base_req <- hf_build_request(
    input = "classify me",
    endpoint_url = server$url("/test_single_sentiment"),
    key_name = "HF_TEST_API_KEY",
    parameters = list()
  )

  response <- httr2::req_perform(base_req)
  expect_equal(httr2::resp_status(response), 200)

  raw_result <- hf_classify_text(
    "classify me",
    endpoint_url = server$url("/test_single_sentiment"),
    key_name = "HF_TEST_API_KEY",
    parameters = list(),
    tidy = FALSE
  )

  expect_s3_class(raw_result, "httr2_response")
  expect_equal(raw_result$status_code, 200)

  tidy_result <- hf_classify_text(
    "classify me",
    endpoint_url = server$url("/test_single_sentiment"),
    key_name = "HF_TEST_API_KEY",
    parameters = list(),
    tidy = TRUE
  )

  # print(tidy_result) # add this so I can see it outside of the test_that context, and verify for myself there's nothing funky with the tests passing when they shouldn't.

  expect_s3_class(tidy_result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(tidy_result), 3)
  expect_true(all(c("positive", "negative", "neutral") %in% names(tidy_result)))
  expect_equal(tidy_result$positive, 0.9, tolerance = 1e-6)
  expect_equal(tidy_result$negative, 0.05, tolerance = 1e-6)
  expect_equal(tidy_result$neutral, 0.05, tolerance = 1e-6)
})

test_that("hf_classify_batch processes a batch of texts and returns a tidied classification response",{

  # test uses the webfake app set up in helper-webfake.R

  .test_url <- server$url("/test_list_sentiment")

  test_response <- httr2::request(.test_url) |>
    httr2::req_method("POST") |>
    httr2::req_perform()


  # test_response |>
  #   httr2::resp_body_string() # uncomment to view response

  parsed_response <- test_response |>
    httr2::resp_body_json()

  # print(parsed_response)

  res <- hf_classify_batch(
    texts = c("1", "2", "3", "4"),
    endpoint_url = .test_url,
    key_name = "HF_TEST_API_KEY",
    max_retries = 1,
    batch_size = 4,
    concurrent_requests = 1,
    include_texts = FALSE
  )


  expect_equal(nrow(res), 4)
  expect_setequal(names(res), c("positive", "negative", "neutral", ".error", ".error_message"))

})


test_that("hf_classify_chunks processes chunks correctly", {
  texts <- paste0("text", 1:6)
  ids <- paste0("id", 1:length(texts))
  temp_dir <- withr::local_tempdir()
  expected_cols <- c("id", "text", ".error", ".error_msg", ".chunk", "positive", "negative", "neutral")

  # Test with chunk_size = 2
  chunk_2 <- expect_no_error(hf_classify_chunks(
    texts = texts,
    ids = ids,
    endpoint_url = server$url("/test_single_sentiment"),
    key_name = "HF_TEST_API_KEY",
    chunk_size = 2,
    concurrent_requests = 1,
    output_dir = temp_dir
  )) |> suppressMessages()

  expect_setequal(unique(chunk_2$`.chunk`), c(1, 2, 3))
  expect_setequal(names(chunk_2), expected_cols)
  expect_equal(nrow(chunk_2), 6)

  # Test with chunk_size = 1
  chunk_1 <- expect_no_error(hf_classify_chunks(
    texts = texts,
    ids = ids,
    endpoint_url = server$url("/test_single_sentiment"),
    key_name = "HF_TEST_API_KEY",
    chunk_size = 1,
    concurrent_requests = 1,
    output_dir = temp_dir
  )) |> suppressMessages()

  expect_setequal(unique(chunk_1$`.chunk`), 1:6)
  expect_equal(nrow(chunk_1), 6)
})

test_that("hf_classify_df works correctly with chunk processing", {
  test_df <- data.frame(
    id = paste0("id", 1:2),
    text = c("text1", "text2"),
    stringsAsFactors = FALSE
  )
  output_dir <- withr::local_tempdir()

  result <- expect_no_error(
    hf_classify_df(
      df = test_df,
      text_var = text,
      id_var = id,
      endpoint_url = server$url("/test_single_sentiment"),
      key_name = "HF_TEST_API_KEY",
      chunk_size = 1,
      output_dir = output_dir
    )
  ) |>
    suppressMessages()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("id", "positive", "negative", "neutral", ".error", ".error_msg", ".chunk") %in% names(result)))
  expect_equal(result$id, c("id1", "id2"))
  expect_equal(result$positive, c(0.9, 0.9), tolerance = 1e-7)
  expect_equal(result$negative, c(0.05, 0.05), tolerance = 1e-7)
  expect_equal(result$neutral, c(0.05, 0.05), tolerance = 1e-7)
  expect_equal(result$.error, c(FALSE, FALSE))
})

test_that("hf_classify_df works with different chunk sizes", {
  test_df <- data.frame(
    id = paste0("id", 1:4),
    text = paste0("text", 1:4),
    stringsAsFactors = FALSE
  )
  temp_dir <- withr::local_tempdir()

  result <- expect_no_error(
    hf_classify_df(
      df = test_df,
      text_var = text,
      id_var = id,
      endpoint_url = server$url("/test_single_sentiment"),
      key_name = "HF_TEST_API_KEY",
      chunk_size = 2,
      concurrent_requests = 1,
      output_dir = temp_dir
    )
  ) |>
    suppressMessages()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_true(all(c("id", ".chunk", ".error", ".error_msg") %in% names(result)))
  expect_equal(result$.error, c(FALSE, FALSE, FALSE, FALSE))
  expect_setequal(unique(result$.chunk), c(1, 2))
})

test_that("hf_classify_df's input validation is working", {

  # safety net for changes
  test_df <- data.frame(
    id = c(1, 2),
    text = c("positive text", "negative text"),
    stringsAsFactors = FALSE
  )

  expect_error(
    hf_classify_df(df = "not_a_dataframe", text_var = text_content, id_var = doc_id, endpoint_url = "url", key_name = "key"),
    "df must be a data frame"
  )

  expect_error(
    hf_classify_df(df = test_df, text_var = text_content, id_var = doc_id, endpoint_url = NULL, key_name = "key"),
    "endpoint_url must be provided"
  )

  expect_error(
    hf_classify_df(df = test_df, text_var = text_content, id_var = doc_id, endpoint_url = "", key_name = "key"),
    "endpoint_url must be provided"
  )

  expect_error(
    hf_classify_df(df = test_df, text_var = text_content, id_var = doc_id, endpoint_url = "url", key_name = "key", concurrent_requests = 0),
    "concurrent_requests must be a number greater than 0"
  )

  expect_error(
    hf_classify_df(df = test_df, text_var = text_content, id_var = doc_id, endpoint_url = "url", key_name = "key", concurrent_requests = "text"),
    "concurrent_requests must be a number greater than 0"
  )

  expect_error(
    hf_classify_df(df = test_df, text_var = text_content, id_var = doc_id, endpoint_url = "url", key_name = "key", chunk_size = "text"),
    "chunk_size must be a number greater than 0"
  )

  expect_error(
    hf_classify_df(df = test_df, text_var = text_content, id_var = doc_id, endpoint_url = "url", key_name = "key", chunk_size = NULL),
    "chunk_size must be a number greater than 0"
  )


})
