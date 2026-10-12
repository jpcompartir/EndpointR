# input validation tests ----

test_that("ant_batch_create validates inputs", {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")

  expect_error(
    ant_batch_create(texts = character(0), custom_ids = character(0)),
    "texts must be a character vector"
  )

  expect_error(
    ant_batch_create(texts = c(1, 2), custom_ids = c("a", "b")),
    "texts must be a character vector"
  )

  expect_error(
    ant_batch_create(texts = c("a", "b"), custom_ids = c("id1")),
    "texts and custom_ids must be the same length"
  )

  expect_error(
    ant_batch_create(texts = c("a", "b"), custom_ids = c("id1", "id1")),
    "custom_ids must be unique"
  )

  expect_error(
    ant_batch_create(
      texts = rep("a", 100001),
      custom_ids = as.character(seq_len(100001))
    ),
    "batch cannot exceed 100,000 requests"
  )

  # too long
  expect_error(
    ant_batch_create(
      texts = c("a", "b"),
      custom_ids = c("short_id", strrep("x", 65))
    ),
    "1-64 characters"
  )

  # invalid characters (colons)
  expect_error(
    ant_batch_create(
      texts = c("a"),
      custom_ids = c("BLUESKY_did:plc:abc123")
    ),
    "letters, numbers, hyphens, and underscores"
  )

  # mix of too long and invalid chars
  expect_error(
    ant_batch_create(
      texts = c("a", "b", "c"),
      custom_ids = c("ok", strrep("a", 100), "has:colons")
    ),
    "2 ids do not meet"
  )
})

test_that("ant_batch_status validates batch_id", {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")

  expect_error(
    ant_batch_status(batch_id = ""),
    "batch_id must be a non-empty character string"
  )

  expect_error(
    ant_batch_status(batch_id = 123),
    "batch_id must be a non-empty character string"
  )

  expect_error(
    ant_batch_status(batch_id = c("a", "b")),
    "batch_id must be a non-empty character string"
  )
})

test_that("ant_batch_list validates limit", {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")

  expect_error(
    ant_batch_list(limit = 0),
    "limit must be between 1 and 1000"
  )

  expect_error(
    ant_batch_list(limit = 1001),
    "limit must be between 1 and 1000"
  )
})

test_that("ant_batch_cancel validates batch_id", {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")

  expect_error(
    ant_batch_cancel(batch_id = ""),
    "batch_id must be a non-empty character string"
  )
})

# .extract_content_from_batch_message tests ----

test_that(".extract_content_from_batch_message extracts text blocks", {
  msg <- list(
    content = list(
      list(type = "text", text = "Hello, world!")
    )
  )

  expect_equal(.extract_content_from_batch_message(msg), "Hello, world!")
})

test_that(".extract_content_from_batch_message handles empty content", {
  msg <- list(content = list())
  expect_true(is.na(.extract_content_from_batch_message(msg)))
})

test_that(".extract_content_from_batch_message handles NULL content", {
  msg <- list(content = NULL)
  expect_true(is.na(.extract_content_from_batch_message(msg)))
})

test_that(".extract_content_from_batch_message handles multiple block types", {
  msg <- list(
    content = list(
      list(type = "tool_use", id = "toolu_1", name = "get_weather"),
      list(type = "text", text = "The answer is 42")
    )
  )

  expect_equal(.extract_content_from_batch_message(msg), "The answer is 42")
})

# mock server integration tests ----

test_that("ant_batch_create sends correct request and returns batch metadata", {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")

  result <- expect_no_error(
    ant_batch_create(
      texts = c("Hello", "World"),
      custom_ids = c("t1", "t2"),
      system_prompt = "Reply in one word",
      endpoint_url = server$url("/test_ant_batch_create")
    )
  )

  expect_equal(result$id, "msgbatch_test123")
  expect_equal(result$processing_status, "in_progress")
  expect_equal(result$request_counts$processing, 2L)
})

test_that(".ant_parse_batch_results parses JSONL correctly into expected tibble", {
  # fetch the JSONL from the mock results endpoint, then parse it as ant_batch_results() does
  raw_text <- httr2::request(server$url("/test_ant_batch_results")) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  result_df <- .ant_parse_batch_results(raw_text)

  expect_s3_class(result_df, "tbl_df")
  expect_equal(nrow(result_df), 2)
  expect_setequal(names(result_df), c("custom_id", "content", ".error", ".error_msg", "stop_reason", "input_tokens", "output_tokens"))

  # check the succeeded row
  succeeded <- result_df[result_df$custom_id == "t1", ]
  expect_equal(succeeded$content, "Hello response")
  expect_false(succeeded$.error)
  expect_equal(succeeded$stop_reason, "end_turn")
  expect_equal(succeeded$input_tokens, 10L)
  expect_equal(succeeded$output_tokens, 5L)

  # check the errored row: the message is nested in result$error$error
  errored <- result_df[result_df$custom_id == "t2", ]
  expect_true(is.na(errored$content))
  expect_true(errored$.error)
  expect_equal(errored$.error_msg, "Invalid request")
})

test_that(".ant_parse_batch_results reads the older flat error shape and empty files", {
  flat_error <- '{"custom_id":"a","result":{"type":"errored","error":{"message":"flat"}}}'
  expect_equal(.ant_parse_batch_results(flat_error)$.error_msg, "flat")

  expired <- '{"custom_id":"b","result":{"type":"expired"}}'
  expect_equal(.ant_parse_batch_results(expired)$.error_msg, "expired")

  expect_equal(nrow(.ant_parse_batch_results("")), 0)
})

test_that("ant_batch_create error messages with curly braces don't crash cli", {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")

  expect_error(
    ant_batch_create(
      texts = c("Hello", "World"),
      custom_ids = c("t1", "t2"),
      endpoint_url = server$url("/test_ant_batch_create_error_with_braces")
    ),
    "Batch creation failed"
  )
})

test_that("ant_batch_results errors when batch not ended", {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")

  expect_error(
    ant_batch_results(
      batch_id = "msgbatch_test123",
      endpoint_url = server$url("/test_ant_batch_status_in_progress")
    ),
    "Batch is not yet complete"
  )
})
