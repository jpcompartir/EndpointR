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

test_that("ant_build_messages_request accepts endpointr_id and adds to headers", {
  req <- ant_build_messages_request(
    "Hello this a test",
    endpointr_id = "id_101"
  )

  expect_equal(req$headers$endpointr_id, "id_101")
})





