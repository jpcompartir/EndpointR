test_that("json_schema class creation works with valid inputs", {
  schema <- json_schema(
    name = "test_schema",
    schema = list(type = "object", properties = list()),
    strict = TRUE,
    description = "A test schema"
  )

  expect_true(S7::S7_inherits(schema, json_schema))

  expect_equal(schema@name, "test_schema")
  expect_equal(schema@schema, list(type = "object", properties = list()))
  expect_true(schema@strict)
  expect_equal(schema@description, "A test schema")
})

test_that("json_schema uses default values correctly", {
  schema <- json_schema(
    name = "test_schema",
    schema = list(type = "object", properties = list())
  )

  expect_true(schema@strict)
  expect_equal(schema@description, "")
})

test_that("json_schema validator catches invalid name", {
  expect_error(
    json_schema(name = "", schema = list()),
    "@name must be a non-empty string"
  )

  expect_error(
    json_schema(name = c("one", "two"), schema = list()),
    "@name must be a non-empty string"
  )

  expect_error(
    json_schema(name = character(0), schema = list()),
    "@name must be a non-empty string"
  )
})

test_that("json_schema validator catches invalid schema", {
  expect_error(
    json_schema(name = "test", schema = "not a list"),
    "@schema must be <list>"
  )

  expect_error(
    json_schema(name = "test", schema = 123),
    "@schema must be <list>"
  )
})

test_that("create_json_schema function works with correct inputs", {
  schema <- create_json_schema(
    name = "person",
    schema = list(
      type = "object",
      properties = list(name = list(type = "string"))
    ),
    strict = FALSE,
    description = "Person schema"
  )

  expect_true(S7::S7_inherits(schema, json_schema))
  expect_equal(schema@name, "person")
  expect_false(schema@strict)
  expect_equal(schema@description, "Person schema")
})

test_that("format_for_api method works correctly", {
  schema <- json_schema(
    name = "api_test",
    schema = list(type = "object", properties = list()),
    strict = FALSE,
    description = "This should not appear in API format"
  )

  result <- json_dump(schema)

  expect_type(result, "list")
  expect_equal(result$json_schema$name, "api_test")
  expect_equal(result$json_schema$schema, list(type = "object", properties = list()))

  expect_false(result$json_schema$strict)
  expect_false("description" %in% names(result))  # Should be excluded
})
