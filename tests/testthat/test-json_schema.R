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

test_that("json_dump method works correctly", {
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

test_that("json_schema validator catches invalid strict value", {
  expect_error(create_json_schema(
    name = "logical test",
    schema = schema_object(
      person = "Olga",
      required = list("person")
    ),
    strict = "character"
  ))

  expect_no_error(
    schema <- create_json_schema(
      name = "logical test",
      schema = schema_object(
        person = "Olga",
        required = list("person")
      ),
      strict = TRUE
    )
  )

})

test_that("json_dump includes description when provided", {
  json_dump_non_empty <- create_json_schema(
    name = "json_dump non-empty?",
    schema = schema_object(
      valid = schema_boolean("empty")
    ),
    description = "non-empty description"
  )

  dumped_description <- json_dump(json_dump_non_empty)$json_schema$description

  expect_equal(nchar(dumped_description), 21)

})

test_that("json_dump has correct structure for API", {
  # Test 'type' field is "json_schema"
  # Test nested type of `schema_object` is 'object'
  # Test nested structure matches API requirements
  dump_api_schema <- create_json_schema(
    name = "dump test",
    schema = schema_object(
      player = schema_string("name of player"),
      club = schema_string("name of club")
    ),
    strict = TRUE
  )

  dumped_api_schema <- json_dump(dump_api_schema)
  expect_equal(dumped_api_schema$type, "json_schema")
  expect_setequal(names(dumped_api_schema), c("type", "json_schema"))

  expect_equal(dumped_api_schema$json_schema$schema$type, "object")
})

# validate_response method
test_that("validate_response handles JSON string input", {
  # Test valid JSON string parsing
  # Test invalid JSON string error

  string_test <- json_schema(
    name = "string test",
    schema = schema_object(
      surname = schema_string(),
      forename = schema_string(),
      status = schema_string()
    )
  )

  invalid_response = list(surname = "Burno",
                          forname = "Fernandes",
                          stauts = 2)

  expect_error(validate_response(string_test,
                      invalid_response),
    regexp = "Response data does not")

  valid_response = list(surname = "Bruno",
                  forename = "Fernandes",
                  status = "not good enough")

  expect_no_error(validate_response(string_test, valid_response))

})

test_that("validate_response rejects non-list, non-string input", {
  schema <- json_schema(
    name = "string test",
    schema = schema_object(
      surname = schema_string(),
      forename = schema_string(),
      status = schema_string()
    )
  )

  non_list_obj <- c(
    surname = "Carrick",
    forename = "Michael",
    status = "Better than Xabi Alonso"
  )

  # fails due to vector in place of list.
  expect_error(validate_response(schema, non_list_obj))

  list_obj <- list(
    surname = "Carrick",
    forename = "Michael",
    status = "Better than Xabi Alonso"
  )

  expect_no_error(validate_response(schema, list_obj))

})

# Schema helper functions
test_that("schema_object creates valid object schemas", {
  # Test required fields included correctly
  # Test additionalProperties defaults to FALSE

  object_schema <- expect_no_error(schema_object(
    city = schema_string(),
    borough = schema_string(),
    required = list("city", "borough")
  ))

  expect_false(object_schema$additionalProperties)
  expect_length(object_schema$required, 2)
  expect_setequal(names(object_schema$properties), c("city", "borough"))

})

test_that("schema_string handles enum constraint", {
  # Test enum values included in schema
  enum_schema <-
    json_schema(
      name = "enum_schema",
      schema =schema_object(
        city = schema_enum(values = c("London", "New York", "Tokyo")),
        rating = schema_enum(values = c(1, 2, 3, 4, 5), type = "integer")
      )
    )

  invalid_city_val <- list(city = "Londin", rating = 5)

  expect_error(
    validate_response(enum_schema, invalid_city_val),
    "Field 'city':")


  invalid_rating_type <- list(city = "London", rating = "five")

  expect_error(
    validate_response(enum_schema, invalid_rating_type), "Field 'rating'")

  invalid_rating_val <- list(city = "London", rating = 6)
  expect_error(
    validate_response(enum_schema, invalid_rating_val),
    "Field 'rating': must be equal")

  valid_response <- list(city = "London", rating = 5)
  expect_no_error(validate_response(enum_schema, valid_response))

})

test_that("schema_number/integer handle range constraints", {
  # Test minimum/maximum included when provided
  # Test constraints work independently
  # Test schema_enum validates types

  numeric_schema <- json_schema(
    name = "numeric values",
    schema = schema_object(
      age = schema_integer(minimum = 18, maximum = 30),
      rating = schema_number(minimum = 3, maximum = 5)
    )
  )

  int_out_of_range <- list(age = 17, rating = 4)

  expect_error(validate_response(numeric_schema, int_out_of_range), "must be >= 18")

  not_int_in_range <- list(age = 20.5, rating = 4)
  expect_error(
    validate_response(numeric_schema, not_int_in_range),
    "must be integer"
  )

  num_out_of_range <- list(age = 21, rating = 2.9)
  expect_error(
    validate_response(numeric_schema, num_out_of_range),
    "must be >= 3"
  )

  expect_no_error(
    validate_response(numeric_schema, list(age = 20, rating = 4))
  )

})

# Integration test
test_that("complete schema creation workflow functions", {
  # Create complex nested schema using helpers
  # Convert to API format with json_dump
  # Verify structure is valid JSON Schema

  league_schema <- create_json_schema(
    name = "league_schema",
    schema = schema_object(
      league = schema_object(
        country = schema_string("country the league is held in"),
        teams = schema_integer("number of teams in the league"),
        awards = schema_object(
          player = schema_string("Player of the year"),
          manager = schema_string("Manager of the year"),
          top_scorer = schema_string("Top scorer in the league"),
          goal = schema_string("Goal of the season")
        ),
        revenue = schema_object(
          television = schema_number("Total TV revenue in millions £ GBP"),
          match_day = schema_number("Total gate receipts in millions £ GBP"),
          merchandise = schema_number("Total merchandise sold in millions £ GBP"),
          subsidies = schema_integer("Government Subsidies in Thousands £ GBP")
        ),
        notable_players = schema_array(
          items = schema_object(
            player_name = schema_string(),
            player_club = schema_string(),
            player_number = schema_integer(minimum = 1, maximum = 99)
          )
        )
      )
    )
  )

  # dump the schema itself:
  dumped_league_schema <- json_dump(league_schema)
  expect_type(dumped_league_schema, "list")
  expect_setequal(names(dumped_league_schema), c("type", "json_schema"))

  expect_equal(dumped_league_schema$json_schema$schema$type, "object")

  expect_setequal(names(dumped_league_schema$json_schema$schema$properties$league$properties), c("country", "teams", "awards", "revenue", "notable_players"))


  # test a response
  england <- list(
    league = list(
      country = "England",
      teams = 20,
      awards = list(
        player = "Salah",
        manager = "Nuno",
        top_scorer = "Salah",
        goal = "Garnacho"
      ),
      revenue = list(
        television = 470.4,
        match_day = 66,
        merchandise = 283.9,
        subsidies = 100
      ),
      notable_players = list(
        list(player_name = "Alejandro Garnacho", player_club = "MUFC", player_number = 49),
        list(player_name = "Trent Alexander-Arnold", player_club = "LFC", player_number = 2)
      )
    )
  )

  # check validating leads to correct outputs:
  validated_resp <- expect_no_error(validate_response(league_schema, england))

  awards <- validated_resp$league$awards
  expect_length(awards, 4)
  expect_setequal(names(awards), c("player", "manager", "top_scorer", "goal"))

  revenue <- validated_resp$league$revenue
  expect_type(revenue$television, "double")


  invalid_data <- england
  invalid_data$league$notable_players[[1]]$player_number <- 100
  expect_error(validate_response(league_schema, invalid_data))

})
