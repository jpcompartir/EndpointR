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
