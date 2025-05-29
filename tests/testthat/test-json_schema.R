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

# Validator edge cases
test_that("json_schema validator catches invalid strict value", {
  # Test non-logical values
  # Test multiple logical values
})

# json_dump method
test_that("json_dump includes description when provided", {
  # Test that non-empty description appears in correct location
})

test_that("json_dump has correct structure for API", {
  # Test 'type' field is "json_schema"
  # Test nested structure matches API requirements
})

# validate_response method
test_that("validate_response handles JSON string input", {
  # Test valid JSON string parsing
  # Test invalid JSON string error
})

test_that("validate_response rejects non-list, non-string input", {
  # Test numeric input fails
  # Test other invalid types fail
})

# Schema helper functions
test_that("schema_object creates valid object schemas", {
  # Test basic object with properties
  # Test required fields included correctly
  # Test additionalProperties defaults to FALSE
})

test_that("schema_string handles enum constraint", {
  # Test enum values included in schema
  # Test description included when provided
})

test_that("schema_number/integer handle range constraints", {
  # Test minimum/maximum included when provided
  # Test constraints work independently
})

test_that("schema_array validates items parameter", {
  # Test min/max items constraints
  # Test nested schemas work as items
})

test_that("schema_enum validates type parameter", {
  # Test invalid type values error
  # Test values match specified type
})

# Integration test
test_that("complete schema creation workflow functions", {
  # Create complex nested schema using helpers
  # Convert to API format with json_dump
  # Verify structure is valid JSON Schema
})
