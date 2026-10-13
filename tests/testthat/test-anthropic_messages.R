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
  expect_equal(req$options$timeout_ms, 120000)

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

  # system prompt should be structured as content block with cache_control for prompt caching
  sys_block <- req_w_sys$body$data$system[[1]]
  expect_equal(sys_block$type, "text")
  expect_equal(sys_block$text, "Talk about all things Peter Pan only")
  expect_equal(sys_block$cache_control, list(type = "ephemeral"))
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


  schema_data <- req_schema$body$data$output_config
  expect_equal(schema_data$format$type, "json_schema")

  expect_equal(names(schema_data$format$schema$properties), "sentiment")


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


test_that("ant_complete_text handles a schema and a system prompt appropriately", {

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
  expected_cols <- c("id", "content", ".error", ".error_msg", "stop_reason", "input_tokens", "output_tokens", "thinking_tokens", ".status", ".chunk")

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
        output_dir = temp_dir,
        overwrite = TRUE # second run into the same directory
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
                  c("id", "content", ".error", ".error_msg", "stop_reason", "input_tokens", "output_tokens", "thinking_tokens", ".status", ".chunk"))
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



test_that("ant_build_messages_request omits temperature when NULL and drops it on models without sampling support", {
  # temperature still included for models that accept sampling (Haiku 4.5)
  req <- ant_build_messages_request("hello", model = "claude-haiku-4-5")
  expect_equal(req$body$data$temperature, 0)

  # NULL: omitted from the body entirely
  req_null <- ant_build_messages_request("hello", model = "claude-haiku-4-5", temperature = NULL)
  expect_false("temperature" %in% names(req_null$body$data))

  # models that reject sampling params (Opus 4.7+, Sonnet 5, Fable 5): dropped with a warning
  rlang::reset_warning_verbosity("ant_sampling_drop")
  expect_warning(
    req_s5 <- ant_build_messages_request("hello", model = "claude-sonnet-5"),
    regexp = "temperature"
  )
  expect_false("temperature" %in% names(req_s5$body$data))
})

test_that("ant_build_messages_request adds effort to output_config", {
  req <- ant_build_messages_request("hello", effort = "high")
  expect_equal(req$body$data$output_config$effort, "high")

  # merges with a schema-derived output_config rather than replacing it
  test_schema <- create_json_schema(
    name = "effort_test",
    schema = schema_object(x = schema_string("a field"), required = list("x"))
  )
  req_both <- ant_build_messages_request("hello", schema = test_schema, effort = "low")
  expect_equal(req_both$body$data$output_config$effort, "low")
  expect_equal(req_both$body$data$output_config$format$type, "json_schema")

  # invalid values rejected
  expect_error(ant_build_messages_request("hello", effort = "extreme"), regexp = "effort")

  # omitted by default
  req_default <- ant_build_messages_request("hello")
  expect_null(req_default$body$data$output_config$effort)
})

# thinking ----

build_body <- function(...) {
  suppressWarnings(ant_build_messages_request("hello", ...))$body$data
}

test_that("ant_build_messages_request leaves thinking out by default, and the default model is Haiku 5.5", {
  body <- build_body()
  expect_equal(body$model, "claude-haiku-5-5")
  expect_null(body$thinking)
  expect_equal(body$max_tokens, 4096L)
})

test_that("ant_build_messages_request builds each thinking setting", {
  expect_equal(build_body(thinking = "adaptive")$thinking, list(type = "adaptive"))
  expect_equal(build_body(thinking = "disabled")$thinking, list(type = "disabled"))
  expect_equal(
    build_body(model = "claude-sonnet-5-5", thinking = "between_tools")$thinking,
    list(type = "between_tools")
  )
  expect_equal(
    build_body(model = "claude-haiku-4-5", thinking = 2048)$thinking,
    list(type = "enabled", budget_tokens = 2048L)
  )

  # effort and thinking together
  both <- build_body(effort = "low", thinking = "adaptive")
  expect_equal(both$output_config$effort, "low")
  expect_equal(both$thinking$type, "adaptive")

  # models EndpointR doesn't know are sent unchecked, e.g. compatible providers
  expect_equal(build_body(model = "deepseek-chat", thinking = "between_tools")$thinking$type, "between_tools")
})

test_that("ant_build_messages_request drops temperature when thinking is on", {
  rlang::reset_warning_verbosity("ant_thinking_temperature_drop")
  expect_warning(
    req <- ant_build_messages_request("hello", model = "claude-haiku-4-5", thinking = 2048),
    "while thinking is on"
  )
  expect_null(req$body$data$temperature)

  rlang::reset_warning_verbosity("ant_thinking_temperature_drop")
  expect_warning(
    req_adaptive <- ant_build_messages_request("hello", model = "claude-sonnet-4-6", thinking = "adaptive"),
    "while thinking is on"
  )
  expect_null(req_adaptive$body$data$temperature)

  # thinking off: temperature stays
  req_off <- ant_build_messages_request("hello", model = "claude-sonnet-4-6", thinking = "disabled")
  expect_equal(req_off$body$data$temperature, 0)
})

test_that("ant_build_messages_request rejects malformed thinking values", {
  expect_error(build_body(thinking = "sometimes"), "must be one of")
  expect_error(build_body(thinking = 2048.5), "whole number")
  expect_error(build_body(thinking = c(1024, 2048)), "whole number")
  expect_error(build_body(thinking = TRUE), "must be")
})

test_that("ant_build_messages_request stops on settings the model rejects", {
  # budget size
  expect_error(build_body(model = "claude-haiku-4-5", thinking = 1000), "at least 1024")
  expect_error(build_body(model = "claude-haiku-4-5", thinking = 4096), "less than `max_tokens`")
  expect_no_error(build_body(model = "claude-haiku-4-5", thinking = 4096, max_tokens = 8192))

  # budget on models from 4.7 onwards
  for (model in c("claude-opus-4-7", "claude-opus-5", "claude-sonnet-5-5", "claude-haiku-5-5", "claude-fable-5-1")) {
    expect_error(build_body(model = model, thinking = 2048), "fixed thinking budget", info = model)
  }
  # still allowed, but deprecated, on the 4.6 models
  expect_no_error(build_body(model = "claude-opus-4-6", thinking = 2048))

  # adaptive on the 4.5 models
  for (model in c("claude-haiku-4-5", "claude-sonnet-4-5", "claude-opus-4-5-20251101")) {
    expect_error(build_body(model = model, thinking = "adaptive"), "Use a fixed budget", info = model)
  }

  # effort on models without it, and levels some models don't have
  expect_error(build_body(model = "claude-haiku-4-5", effort = "low"), "doesn't accept `effort`")
  expect_error(build_body(model = "claude-sonnet-4-5", effort = "low"), "doesn't accept `effort`")
  expect_no_error(build_body(model = "claude-opus-4-5", effort = "low"))
  expect_error(build_body(model = "claude-opus-4-6", effort = "xhigh"), "xhigh")
  expect_no_error(build_body(model = "claude-opus-4-6", effort = "max"))
  expect_error(build_body(model = "claude-opus-4-5", effort = "max"), "max")

  # disabled where thinking can't be turned off
  for (model in c("claude-opus-5-5", "claude-fable-5-1", "claude-mythos-5")) {
    expect_error(build_body(model = model, thinking = "disabled"), "can't be turned off", info = model)
  }
  expect_error(build_body(model = "claude-sonnet-5-5", thinking = "disabled"), "between_tools")

  # between_tools anywhere but Sonnet 5.5
  expect_error(build_body(model = "claude-opus-4-8", thinking = "between_tools"), "Only Claude Sonnet 5.5")

  # thinking off at xhigh or max on Opus 5, Haiku 5.5 and Sonnet 5.5
  expect_error(build_body(model = "claude-opus-5", effort = "xhigh", thinking = "disabled"), "\"high\" or lower")
  expect_error(build_body(model = "claude-haiku-5-5", effort = "max", thinking = "disabled"), "\"high\" or lower")
  expect_error(
    build_body(model = "claude-sonnet-5-5", effort = "max", thinking = "between_tools"),
    "\"high\" or lower"
  )
  expect_no_error(build_body(model = "claude-opus-5", effort = "high", thinking = "disabled"))
  # Opus 4.8 and Sonnet 5 accept disabled at any effort
  expect_no_error(build_body(model = "claude-opus-4-8", effort = "max", thinking = "disabled"))
  expect_no_error(build_body(model = "claude-sonnet-5", effort = "xhigh", thinking = "disabled"))
})

test_that("ant_complete_df returns the usage columns", {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")

  result <- ant_complete_df(
    get_review_df(),
    review_text,
    id,
    endpoint_url = server$url("/test_ant_complete_df_review"),
    concurrent_requests = 1,
    output_dir = withr::local_tempdir()
  ) |>
    suppressMessages() |>
    suppressWarnings()

  expect_equal(unique(result$stop_reason), "end_turn")
  expect_equal(unique(result$input_tokens), 12L)
  expect_equal(unique(result$output_tokens), 40L)
  expect_equal(unique(result$thinking_tokens), 30L)
})

test_that("both Messages routes warn when a response ran out of tokens while thinking", {
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")
  url <- server$url("/test_ant_out_of_tokens")

  expect_warning(
    content <- ant_complete_text("hello", endpoint_url = url, temperature = NULL),
    "ran out of tokens"
  )
  expect_true(is.na(content))

  expect_warning(
    result <- ant_complete_chunks(
      texts = c("a", "b"),
      ids = c("1", "2"),
      endpoint_url = url,
      concurrent_requests = 1,
      temperature = NULL,
      output_dir = withr::local_tempdir()
    ) |> suppressMessages(),
    "2 responses ran out of tokens"
  )
  expect_equal(result$stop_reason, c("max_tokens", "max_tokens"))
  expect_equal(result$thinking_tokens, c(64L, 64L))
})

test_that("ant_complete_text passes key_name through to the request", {
  withr::local_envvar(ANTHROPIC_API_KEY = NA, MY_ANT_API_KEY = "test-key")
  expect_no_error(
    ant_complete_text(
      "hello",
      key_name = "MY_ANT_API_KEY",
      endpoint_url = server$url("/test_ant_schemaless"),
      temperature = NULL
    )
  )
})
