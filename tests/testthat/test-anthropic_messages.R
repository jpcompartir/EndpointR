test_that("ant_build_messages_request validates inputs and generates valid requests", {

  expect_error(
    ant_build_messages_request(input = c("Vector", "input")),
    "input must be a non-empty character string"
  )

  expect_error(
    ant_build_messages_request(input = "User stuff", system_prompt = c("Vector", "Prompt")), "must be a <character>"
  )

  req <- expect_no_error(
    ant_build_messages_request(input = "Test Input Alone")
  )

  expect_equal(req$headers$`Content-Type`, "application/json")
  expect_equal(req$headers$`anthropic-version`, "2023-06-01")
  expect_equal(req$body$data$messages[[1]][["content"]], "Test Input Alone")

  expect_equal(req$url, "https://api.anthropic.com/v1/messages")
  expect_equal(req$method, "POST")
  expect_equal(req$policies$retry_max_tries, 5)
  expect_equal(req$options$timeout_ms, 30000)

  expect_error(
    ant_build_messages_request("hello",temperature = 2),
    "temperature must be numeric between 0 and 1"
  )
})

test_that("ant_build_messages accepts a system_prompt and the request is formatted appropriately", {

  message <- "The 4th king of neverland was not Captain Hook"
  req <- ant_build_messages_request(message)

  expect_null(req$body$data$system)

  req_w_sys <- ant_build_messages_request(message, system_prompt = "Talk about all things Peter Pan only")

  expect_true(!is.null(req_w_sys$body$data$system))
})

test_that("ant_build_messages_request accepts schemas and formats properly with .ant_format_schema", {
  sentiment_schema <- create_json_schema(
    name = "sent_schema",
    schema = schema_object(
      sentiment = schema_enum(values = c("positive", "negative", "neutral")),
      required = list("sentiment"),
      additional_properties = FALSE
    )
  )

  req_schema <-ant_build_messages_request(
    "the UX of tensorflow was vastly inferior to Pytorch, hence the latter's dominance",
    schema =  sentiment_schema,
    model = "claude-sonnet-4-5")


  schema_data <- req_schema$body$data$output_format
  expect_equal(schema_data$type, "json_schema")

  expect_equal(names(schema_data$schema$properties), "sentiment")
  expect_equal(req_schema$headers$`anthropic-beta`, "structured-outputs-2025-11-13")


})

test_that(".extract_ant_message_content covers basic edgecases and actually gets the text", {
  mock_body <- list(
    content = list(
      list(type = "text", text = "Hello, world!")
    ),
    stop_reason = "end_turn"
  )

  mock_response <- httr2::response_json(
    # status_code = 200L,
    body = mock_body
  )

  content <- .extract_ant_message_content(mock_response)
  expect_equal(content, "Hello, world!")

  mock_empty_body <- list(
    content = list(),
    stop_reason = "end_turn"
  )

  mock_empty_response <- httr2::response_json(
    # status_code = 200L,
    body = mock_empty_body
  )

  empty_content <- .extract_ant_message_content(mock_empty_response)
  expect_true(is.na(empty_content))

})


test_that("ant_build_messages_request accepts endpointr_id and adds to headers", {
  req <- ant_build_messages_request(
    "Hello this a test",
    endpointr_id = "id_101"
  )

  expect_equal(req$headers$endpointr_id, "id_101")
})


test_that("ant_complete_text validates its inputs", {

  expect_error(ant_complete_text(c("")),
               "must not be an empty")

  expect_error(ant_complete_text(c("hello", "bonjour")),
               "must be a single string")
})


test_that("ant_complete_text takes a single text and returns the response", {

  test_url <- server$url("/test_ant_schemaless")

  withr::with_envvar(
    c("ANTHROPIC_API_KEY" = "test-key"),
    {
      response <- expect_no_error(ant_complete_text(
        text = "Give me a helpful response",
        endpoint_url = test_url
      ))
    }
  )

  expect_true(grepl(x = response, pattern = "helpful response"))
})


test_that("ant_complete_text handles a schema appropriately", {

  test_url <- server$url("/test_ant_sentiment")

  sentiment_schema <- create_json_schema(
    name = "sentiment_schema",
    schema = schema_object(
      sentiment = schema_enum(values = c("positive", "negative", "neutral"), type = "string"),
      confidence = schema_number(minimum = 0, maximum = 1)
    )
  )

  withr::with_envvar(
    c("ANTHROPIC_API_KEY" = "test-key"),
    {
      schema_response <- expect_no_error(
        ant_complete_text(
          text = "What a remarkable achievement",
          system_prompt = "classify the sentiment of this text: ",
          endpoint_url = test_url,
          schema = sentiment_schema,
          tidy = FALSE
        ))
    }
  )

  expect_no_error(validate_response(sentiment_schema, schema_response[[1]]))
})


test_that("ant_complete_chunks processes chunks correctly", {
  texts <- paste0("text", 1:6)
  ids <- paste0("id", 1:length(texts))
  temp_dir <- withr::local_tempdir()
  expected_cols <- c("id", "content", ".error", ".error_msg", ".status", ".chunk")

  withr::with_envvar(
    c("ANTHROPIC_API_KEY" = "test-key"),
    {
      # Test with chunk_size = 2
      chunk_2 <- expect_no_error(ant_complete_chunks(
        texts = texts,
        ids = ids,
        endpoint_url = server$url("/test_ant_complete_df_review"),
        key_name = "ANTHROPIC_API_KEY",
        chunk_size = 2,
        concurrent_requests = 1,
        output_dir = temp_dir
      )) |> suppressMessages()
    }
  )

  expect_setequal(unique(chunk_2$.chunk), c(1, 2, 3))
  expect_setequal(names(chunk_2), expected_cols)
  expect_equal(nrow(chunk_2), 6)

  withr::with_envvar(
    c("ANTHROPIC_API_KEY" = "test-key"),
    {
      # Test with chunk_size = 1
      chunk_1 <- expect_no_error(ant_complete_chunks(
        texts = texts,
        ids = ids,
        endpoint_url = server$url("/test_ant_complete_df_review"),
        key_name = "ANTHROPIC_API_KEY",
        chunk_size = 1,
        concurrent_requests = 1,
        output_dir = temp_dir
      )) |> suppressMessages()
    }
  )

  expect_setequal(unique(chunk_1$.chunk), 1:6)
  expect_equal(nrow(chunk_1), 6)
})


test_that("ant_complete_df takes single row, multi-row data frames as inputs", {
  expect_error(ant_complete_df("hello"),
               regexp = "df must be")

  review_df <- get_review_df()

  endpoint_url <- server$url("/test_ant_complete_df_review")

  withr::with_envvar(
    c("ANTHROPIC_API_KEY" = "test-key"),
    {
      successful_response <- expect_no_error(
        ant_complete_df(review_df,
                        review_text,
                        id,
                        endpoint_url = endpoint_url,
                        concurrent_requests = 1,
                        max_retries = 1,
                        output_dir = NULL)
      )
    }
  )

  expect_setequal(names(successful_response),
                  c("id", "content", ".error", ".error_msg", ".status", ".chunk"))
  expect_setequal(unique(successful_response$content), "positive")

  withr::with_envvar(
    c("ANTHROPIC_API_KEY" = "test-key"),
    {
      expect_message(object =
                       ant_complete_df(review_df,
                                       review_text,
                                       id,
                                       endpoint_url = endpoint_url,
                                       concurrent_requests = 1,
                                       max_retries = 1,
                                       output_dir = NULL),
                     regexp = "Processing 5 text"
      )
    }
  )
})


test_that("ant_complete_df works correctly with chunk processing", {
  test_df <- data.frame(
    id = paste0("id", 1:2),
    text = c("text1", "text2"),
    stringsAsFactors = FALSE
  )
  output_dir <- withr::local_tempdir()

  withr::with_envvar(
    c("ANTHROPIC_API_KEY" = "test-key"),
    {
      result <- expect_no_error(
        ant_complete_df(
          df = test_df,
          text_var = text,
          id_var = id,
          endpoint_url = server$url("/test_ant_complete_df_review"),
          key_name = "ANTHROPIC_API_KEY",
          chunk_size = 1,
          output_dir = output_dir
        )
      ) |> suppressMessages()
    }
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("id", "content", ".error", ".error_msg", ".chunk") %in% names(result)))
  expect_equal(result$id, c("id1", "id2"))
  expect_equal(result$content, c("positive", "positive"))
  expect_equal(result$.error, c(FALSE, FALSE))
})


test_that("ant_complete_df works with different chunk sizes", {
  test_df <- data.frame(
    id = paste0("id", 1:4),
    text = paste0("text", 1:4),
    stringsAsFactors = FALSE
  )
  temp_dir <- withr::local_tempdir()

  withr::with_envvar(
    c("ANTHROPIC_API_KEY" = "test-key"),
    {
      result <- expect_no_error(
        ant_complete_df(
          df = test_df,
          text_var = text,
          id_var = id,
          endpoint_url = server$url("/test_ant_complete_df_review"),
          key_name = "ANTHROPIC_API_KEY",
          chunk_size = 2,
          concurrent_requests = 1,
          output_dir = temp_dir
        )
      ) |> suppressMessages()
    }
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_true(all(c("id", ".chunk", ".error", ".error_msg") %in% names(result)))
  expect_equal(result$.error, c(FALSE, FALSE, FALSE, FALSE))
  expect_setequal(unique(result$.chunk), c(1, 2))
})


test_that("ant_complete_df takes a schema as input", {

  sentiment_schema <- create_json_schema(
    name = "sentiment_test",
    schema = schema_object(
      sentiment = schema_enum(
        values = c("positive", "negative", "neutral"),
        description = "Sentiment classification for the document",
        type = "string"
      ),
      required = list("sentiment"),
      additional_properties = FALSE
    )
  )

  review_df <- get_review_df()

  endpoint_url <- server$url("/test_ant_complete_df_schema")

  withr::with_envvar(
    c("ANTHROPIC_API_KEY" = "test-key"),
    {
      successful_response <- expect_no_error(
        ant_complete_df(review_df,
                        review_text,
                        id,
                        endpoint_url = endpoint_url,
                        concurrent_requests = 1,
                        max_retries = 1,
                        schema = sentiment_schema,
                        output_dir = NULL
        )
      )
    }
  )

  expect_s3_class(successful_response, "data.frame")
  expect_equal(nrow(successful_response), 5)
  expect_true("content" %in% names(successful_response))
  expect_true(all(grepl("sentiment", successful_response$content)))
})


test_that("ant_complete_df's input validation is working", {

  test_df <- data.frame(
    id = c(1, 2),
    text = c("positive text", "negative text"),
    stringsAsFactors = FALSE
  )

  expect_error(
    ant_complete_df(df = "not_a_dataframe", text_var = text, id_var = id, endpoint_url = "url", key_name = "key"),
    "df must be a data frame"
  )

  expect_error(
    ant_complete_df(df = data.frame(), text_var = text, id_var = id, endpoint_url = "url", key_name = "key"),
    "df must not be empty"
  )

  expect_error(
    ant_complete_df(df = test_df, text_var = text, id_var = id, chunk_size = "text", endpoint_url = "url", key_name = "key"),
    "`chunk_size` must be a positive integer"
  )

  expect_error(
    ant_complete_df(df = test_df, text_var = text, id_var = id, chunk_size = NULL, endpoint_url = "url", key_name = "key"),
    "`chunk_size` must be a positive integer"
  )
})


