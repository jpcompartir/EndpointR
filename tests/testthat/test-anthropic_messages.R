test_that("Building Anthropic Messages Requests works with arguments and features", {

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


})


test_that("Anthropic Messages Requests with schemas look right", {
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



