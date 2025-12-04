utils::globalVariables(c(".embeddings", ".request", ".response", ".row_num", ".data", ".error",
                         ".error_msg", ".status", "original_index", "text", ":=", ".row_id", "id", "label", "score", "verbose"))

.onLoad <- function(...) {
  S7::methods_register()
}
