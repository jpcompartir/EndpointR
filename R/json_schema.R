#' Create JSON Schema S7 class for structured outputs
#'
#' @description
#' S7 class for JSON Schema definitions used in OpenAI structured outputs
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

# S7 generic for formatting schema for API
format_for_api <- S7::new_generic("format_for_api", "x")

#' Format json_schema for API request
#' @name format_for_api
#' @export
S7::method(format_for_api, json_schema) <- function(x) {
  list(
    name = x@name,
    schema = x@schema,
    strict = x@strict
  )
}
