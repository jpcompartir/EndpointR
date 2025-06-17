#' @importFrom jsonvalidate json_validator
NULL

#' Create JSON Schema S7 class for structured outputs
#'
#' @description
#' S7 class for JSON Schema definitions used in, e.g. OpenAI structured outputs
#' @param name Name of the schema
#' @param schema The JSON schema definition as a list
#' @param strict Whether to enforce strict mode (default TRUE)
#' @param description Optional description of the schema
#' @export
json_schema <- S7::new_class(
  "json_schema",
  properties = list(
    name = S7::new_property(class = S7::class_character),
    schema = S7::new_property(class = S7::class_list),
    strict = S7::new_property(class = S7::class_logical, default = TRUE),
    description = S7::new_property(class = S7::class_character, default = "")
  ),
  validator = function(self) {
    if (length(self@name) != 1 || nchar(self@name) == 0) {
      "@name must be a non-empty string"
    } else if (!is.list(self@schema)) {
      "@schema must be a list"
    } else if (length(self@strict) != 1) {
      "@strict must be a single logical value"
    }
  }
)

#' Create a JSON Schema object
#'
#' @param name Name of the schema
#' @param schema The JSON schema definition as a list
#' @param strict Whether to enforce strict mode (default TRUE)
#' @param description Optional description of the schema
#'
#' @return A json_schema S7 object
#' @export
create_json_schema <- function(name, schema, strict = TRUE, description = "") {
  json_schema(
    name = name,
    schema = schema,
    strict = strict,
    description = description
  )
}

# S7 generics for JSON schema operations
json_dump <- S7::new_generic("json_dump", "x")
validate_response <- S7::new_generic("validate_response", "schema")

#' Convert json_schema to API format
#' @name json_dump
#' @export
S7::method(json_dump, json_schema) <- function(x) {
  result <- list(
    type = "json_schema",
    json_schema = list(
      name = x@name,
      schema = x@schema,
      strict = x@strict
    )
  )

  if (nchar(x@description) > 0) {
    result$json_schema$description <- x@description
  }

  result
}

#' Validate response data against schema
#' @name validate_response
#' @export
S7::method(validate_response, json_schema) <- function(schema, data) {
  if (!is.list(data) && !is.character(data)) {
    cli::cli_abort("Response data must be a list or JSON string")
  }

  if (is.character(data)) {
    data <- jsonlite::fromJSON(data, simplifyVector = FALSE)
  }

  schema_json <- jsonlite::toJSON(schema@schema, auto_unbox = TRUE)
  data_json <- jsonlite::toJSON(data, auto_unbox = TRUE)

  # ajv engine with verbose for getting error messages out
  is_valid <- jsonvalidate::json_validate(data_json, schema_json,
                                          engine = "ajv", verbose = TRUE)

  if (!is_valid) {
    errors <- attr(is_valid, "errors")

    error_msgs <- errors |>
      dplyr::mutate(
        field = dplyr::case_when(
          instancePath == "" ~ "root",
          TRUE ~ stringr::str_remove(instancePath, "^/")
        )
      ) |>
      dplyr::mutate(
        error_text = glue::glue("Field '{field}': {message}")
      ) |>
      dplyr::pull(error_text)

    cli::cli_abort(c(
      "Response data does not match schema:",
      "x" = error_msgs
    ))
  }

  return(data)
}


#' Create JSON Schema object definitions
#'
#' @description
#' These helper functions simplify creating JSON Schema definitions for
#' structured outputs (e.g. OpenAI). They provide type-safe, validated schemas that ensure
#' consistent LLM responses matching your expected data structure.
#'
#' JSON Schema constrains LLM outputs to specific formats, preventing parsing
#' errors and ensuring reliable data extraction from unstructured text.

#' Create object schema with nested properties
#'
#' @description
#' Defines a JSON object with typed properties. Use this for structured data
#' like user profiles, API responses, or any nested data structure. The LLM
#' will return JSON matching exactly this schema.
#'
#' @param ... Named arguments defining object properties (use other schema_* helpers)
#' @param required Character vector of required property names
#' @param additional_properties Whether to allow extra properties beyond those defined
#'
#' @examples
#' # User profile with required fields
#' schema_object(
#'   name = schema_string("Full name"),
#'   age = schema_integer("Age in years"),
#'   required = c("name", "age")
#' )
#'
#' @export
schema_object <- function(..., required = NULL, additional_properties = FALSE) {
  properties <- list(...)

  schema <- list(
    type = "object",
    properties = properties,
    additionalProperties = additional_properties
  )

  if (!is.null(required)) {
    schema$required <- required
  }

  schema
}

#' Create string property schema
#'
#' @description
#' Defines text fields with optional constraints. Use for names, descriptions,
#' or any textual data. Can restrict to specific values via enum parameter.
#'
#' @param description Human-readable field description (helps LLM understand context)
#' @param enum Character vector of allowed values (creates dropdown-like constraint)
#'
#' @examples
#' # Simple text field
#' schema_string("User's email address")
#'
#' # Constrained to specific values
#' schema_string("Sentiment", enum = c("positive", "negative", "neutral"))
#'
#' @export
schema_string <- function(description = NULL, enum = NULL) {
  schema <- list(type = "string")
  if (!is.null(description)) schema$description <- description
  if (!is.null(enum)) schema$enum <- enum
  schema
}

#' Create numeric property schema
#'
#' @description
#' Defines decimal number fields with optional range constraints. Use for
#' prices, percentages, ratings, or any continuous numeric data.
#'
#' @param description Human-readable field description
#' @param minimum Minimum allowed value (inclusive)
#' @param maximum Maximum allowed value (inclusive)
#'
#' @examples
#' # Product price
#' schema_number("Price in USD", minimum = 0)
#'
#' # Percentage score
#' schema_number("Confidence score", minimum = 0, maximum = 100)
#'
#' @export
schema_number <- function(description = NULL, minimum = NULL, maximum = NULL) {
  schema <- list(type = "number")
  if (!is.null(description)) schema$description <- description
  if (!is.null(minimum)) schema$minimum <- minimum
  if (!is.null(maximum)) schema$maximum <- maximum
  schema
}

#' Create integer property schema
#'
#' @description
#' Defines whole number fields with optional range constraints. Use for counts,
#' IDs, quantities, or discrete numeric values. More restrictive than schema_number.
#'
#' @param description Human-readable field description
#' @param minimum Minimum allowed value (inclusive)
#' @param maximum Maximum allowed value (inclusive)
#'
#' @examples
#' # Item quantity
#' schema_integer("Quantity ordered", minimum = 1)
#'
#' # Rating scale
#' schema_integer("Star rating", minimum = 1, maximum = 5)
#'
#' @export
schema_integer <- function(description = NULL, minimum = NULL, maximum = NULL) {
  schema <- list(type = "integer")
  if (!is.null(description)) schema$description <- description
  if (!is.null(minimum)) schema$minimum <- minimum
  if (!is.null(maximum)) schema$maximum <- maximum
  schema
}

#' Create boolean property schema
#'
#' @description
#' Defines true/false fields. Use for flags, yes/no questions, or binary states.
#' Ensures LLM returns exactly true or false, not "yes"/"no" strings.
#'
#' @param description Human-readable field description
#'
#' @examples
#' # Feature flag
#' schema_boolean("Has premium subscription")
#'
#' # Classification result
#' schema_boolean("Contains sensitive information")
#'
#' @export
schema_boolean <- function(description = NULL) {
  schema <- list(type = "boolean")
  if (!is.null(description)) schema$description <- description
  schema
}

#' Create enumerated property schema
#'
#' @description
#' Defines fields constrained to specific allowed values. More flexible than
#' schema_string with enum parameter - supports numeric enums and mixed types.
#' Use for categories, status codes, or any multi-choice field.
#'
#' @param values Vector of allowed values
#' @param description Human-readable field description
#' @param type Data type of enum values ("string", "integer", or "number")
#'
#' @examples
#' # Status categories
#' schema_enum(c("draft", "published", "archived"), "Document status")
#'
#' # Priority levels as numbers
#' schema_enum(c(1, 2, 3, 4, 5), "Priority level", type = "integer")
#'
#' @export
schema_enum <- function(values, description = NULL, type = "string") {
  if (!type %in% c("string", "integer", "number")) {
    cli::cli_abort("Enum type must be 'string', 'integer', or 'number'")
  }

  schema <- list(
    type = type,
    enum = values
  )
  if (!is.null(description)) schema$description <- description
  schema
}

#' Create array property schema
#'
#' @description
#' Defines list/array fields containing multiple items of the same type. Use for
#' tags, categories, or any collection data. Can constrain array length.
#'
#' @param items Schema definition for array elements (use other schema_* helpers)
#' @param description Human-readable field description
#' @param min_items Minimum number of array elements
#' @param max_items Maximum number of array elements
#'
#' @examples
#' # List of tags
#' schema_array(schema_string(), "Product tags", max_items = 10)
#'
#' # Array of objects
#' schema_array(
#'   schema_object(
#'     name = schema_string("Ingredient name"),
#'     amount = schema_number("Amount needed")
#'   ),
#'   "Recipe ingredients"
#' )
#'
#' @export
schema_array <- function(items, description = NULL, min_items = NULL, max_items = NULL) {
  schema <- list(type = "array", items = items)
  if (!is.null(description)) schema$description <- description
  if (!is.null(min_items)) schema$minItems <- min_items
  if (!is.null(max_items)) schema$maxItems <- max_items
  schema
}
