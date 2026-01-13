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

test_that("oai_batch_build_completions_req creates valid JSON structure", {
  result <- oai_batch_build_completions_req(
    input = "Hello",
    id = "test_1",
    model = "gpt-4o-mini"
  )

  parsed <- jsonlite::fromJSON(result, simplifyVector = FALSE)

  expect_equal(parsed$custom_id, "test_1")
  expect_equal(parsed$method, "POST")
  expect_equal(parsed$url, "/v1/chat/completions")
  expect_equal(parsed$body$model, "gpt-4o-mini")
  expect_equal(length(parsed$body$messages), 1)
  expect_equal(parsed$body$messages[[1]]$role, "user")
  expect_equal(parsed$body$messages[[1]]$content, "Hello")
})

test_that("oai_batch_build_completions_req handles system_prompt", {
  result <- oai_batch_build_completions_req(
    input = "Hello",
    id = "test_2",
    system_prompt = "You are helpful"
  )

  parsed <- jsonlite::fromJSON(result, simplifyVector = FALSE)

  expect_equal(length(parsed$body$messages), 2)
  expect_equal(parsed$body$messages[[1]]$role, "system")
  expect_equal(parsed$body$messages[[1]]$content, "You are helpful")
  expect_equal(parsed$body$messages[[2]]$role, "user")
})

test_that("oai_batch_build_completions_req handles schema as list", {
  test_schema <- list(
    type = "json_schema",
    json_schema = list(
      name = "test",
      schema = list(type = "object", properties = list(sentiment = list(type = "string")))
    )
  )

  result <- oai_batch_build_completions_req(
    input = "Hello",
    id = "test_3",
    schema = test_schema
  )

  parsed <- jsonlite::fromJSON(result)

  expect_true("response_format" %in% names(parsed$body))
  expect_equal(parsed$body$response_format$type, "json_schema")
})

test_that("oai_batch_build_completions_req respects temperature and max_tokens", {
  result <- oai_batch_build_completions_req(
    input = "Hello",
    id = "test_4",
    temperature = 0.7,
    max_tokens = 1000L
  )

  parsed <- jsonlite::fromJSON(result)

  expect_equal(parsed$body$temperature, 0.7)
  expect_equal(parsed$body$max_tokens, 1000)
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

test_that("oai_batch_prepare_completions handles system_prompt across all rows", {
  test_df <- tibble::tibble(
    id = c("a", "b"),
    text = c("Hello", "World")
  )

  result <- oai_batch_prepare_completions(
    df = test_df,
    text_var = text,
    id_var = id,
    system_prompt = "Be brief"
  )

  lines <- strsplit(result, "\n")[[1]]
  parsed <- purrr::map(lines, \(x) jsonlite::fromJSON(x, simplifyVector = FALSE))

  expect_equal(parsed[[1]]$body$messages[[1]]$role, "system")
  expect_equal(parsed[[2]]$body$messages[[1]]$role, "system")
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


test_that("oai_batch_parse_completions handles success response", {
  mock_content <- '{"custom_id":"1","response":{"body":{"choices":[{"message":{"content":"Hello back"}}]}},"error":null}'

  result <- oai_batch_parse_completions(mock_content)

  expect_equal(nrow(result), 1)
  expect_equal(result$custom_id, "1")
  expect_equal(result$content, "Hello back")
  expect_false(result$.error)
})

test_that("oai_batch_parse_completions handles error response", {
  mock_content <- '{"custom_id":"1","response":null,"error":{"message":"API error"}}'

  result <- oai_batch_parse_completions(mock_content)

  expect_equal(nrow(result), 1)
  expect_true(result$.error)
  expect_equal(result$.error_msg, "API error")
  expect_true(is.na(result$content))
})

test_that("oai_batch_parse_completions handles JSON schema content", {
  mock_content <- '{"custom_id":"1","response":{"body":{"choices":[{"message":{"content":"{\\"sentiment\\":\\"positive\\"}"}}]}},"error":null}'

  result <- oai_batch_parse_completions(mock_content)

  expect_equal(result$content, '{"sentiment":"positive"}')
  parsed_content <- jsonlite::fromJSON(result$content)
  expect_equal(parsed_content$sentiment, "positive")
})

test_that("oai_batch_parse_completions renames id column when original_df provided", {
  mock_content <- '{"custom_id":"doc_1","response":{"body":{"choices":[{"message":{"content":"test"}}]}},"error":null}'

  original_df <- tibble::tibble(
    my_id = "doc_1",
    text = "Hello"
  )

  result <- oai_batch_parse_completions(mock_content, original_df, id_var = "my_id")

  expect_true("my_id" %in% names(result))
  expect_false("custom_id" %in% names(result))
  expect_equal(result$my_id, "doc_1")
})

test_that("oai_batch_prepare_embeddings rejects duplicate IDs", {
  test_df <- tibble::tibble(
    id = c("a", "a", "b"),
    text = c("Text 1", "Text 2", "Text 3")
  )

  expect_error(
    oai_batch_prepare_embeddings(test_df, text, id),
    "custom_id values must be unique"
  )
})

test_that("oai_batch_prepare_completions rejects duplicate IDs", {
  test_df <- tibble::tibble(
    id = c("x", "y", "x"),
    text = c("Hello", "World", "Again")
  )

  expect_error(
    oai_batch_prepare_completions(test_df, text, id),
    "custom_id values must be unique"
  )
})

test_that("oai_batch_prepare_embeddings handles empty dataframe with warning", {
  test_df <- tibble::tibble(id = character(), text = character())

  expect_warning(
    result <- oai_batch_prepare_embeddings(test_df, text, id),
    "Input is empty"
  )
  expect_equal(result, "")
})

test_that("oai_batch_prepare_completions handles empty dataframe with warning", {
  test_df <- tibble::tibble(id = character(), text = character())

  expect_warning(
    result <- oai_batch_prepare_completions(test_df, text, id),
    "Input is empty"
  )
  expect_equal(result, "")
})

test_that("oai_batch_parse_embeddings handles empty input", {
  result <- oai_batch_parse_embeddings("")
  expect_equal(nrow(result), 0)
  expect_true("custom_id" %in% names(result))
  expect_true(".error" %in% names(result))
})

test_that("oai_batch_parse_completions handles empty input", {
  result <- oai_batch_parse_completions("")
  
  expect_equal(nrow(result), 0)
  expect_true("custom_id" %in% names(result))
  expect_true("content" %in% names(result))
})
