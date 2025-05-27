utils::globalVariables(c(".embeddings", ".request", ".response", ".row_num", ".data", ".error",
                         ".error_message", "original_index", "text", ":="))

.onLoad <- function(...) {
  S7::methods_register()
}
