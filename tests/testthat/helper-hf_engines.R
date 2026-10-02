# Fake Hugging Face endpoints for the two inference engines.
#
# TEI rules copied from Text Embeddings Inference 1.8.2:
#   - GET /info returns the model and its limits
#   - a flat list of 2 strings is read as one sentence pair (one result, no error)
#   - a flat list of 3 or more strings is rejected
#   - more inputs than max_client_batch_size are rejected
#   - a NaN score with raw_scores = true returns 424 for the whole request
#   - classifier labels are sorted by score
# The status codes for the two rejections (422 for the JSON error, 413 for the
# batch size) come from TEI's source code and have not been checked against a
# live endpoint.
#
# Toolkit rules: no /info route, and one empty text fails the whole batch.

.hf_app <- webfakes::new_app()
.hf_app$use(webfakes::mw_json())

# the app runs in another process, so handlers reach helpers through app$locals
.l <- .hf_app$locals
.l$tei_max_batch <- 4L

.l$tei_info <- function(model_type) {
  list(
    model_id = "/repository",
    model_sha = NULL,
    model_dtype = "float16",
    model_type = model_type,
    max_concurrent_requests = 512L,
    max_input_length = 8192L,
    max_batch_tokens = 65536L,
    max_client_batch_size = 4L,
    auto_truncate = TRUE,
    version = "1.8.2"
  )
}

.l$send_json <- function(res, body, status = 200L) {
  res$set_status(status)$
    set_header("Content-Type", "application/json")$
    send(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", digits = NA))
}

# spam logit grows with text length, so results can be traced back to their text
.l$tei_logits <- function(text) {
  scores <- list(
    list(label = "not_spam", score = 0),
    list(label = "spam", score = nchar(text) / 10 - 1)
  )
  scores[order(-purrr::map_dbl(scores, "score"))]
}

.hf_app$get("/tei_classify/info", function(req, res) {
  l <- req$app$locals
  l$send_json(res, l$tei_info(list(classifier = list(id2label = list(`0` = "not_spam", `1` = "spam")))))
})

.hf_app$post("/tei_classify", function(req, res) {
  l <- req$app$locals
  inputs <- req$json$inputs

  if (is.character(inputs) && length(inputs) == 1) {
    return(l$send_json(res, l$tei_logits(inputs)))
  }

  is_flat <- all(purrr::map_lgl(inputs, is.character))
  if (is_flat && length(inputs) == 2) {
    # read as a sentence pair: one result
    return(l$send_json(res, l$tei_logits(paste(inputs, collapse = " "))))
  }
  if (is_flat && length(inputs) >= 3) {
    return(l$send_json(res, list(
      error = "Failed to deserialize the JSON body into the target type: inputs: expected a string, a pair of strings [string, string] or a batch of mixed strings and pairs",
      error_type = "Validation"
    ), status = 422L))
  }
  if (length(inputs) > l$tei_max_batch) {
    return(l$send_json(res, list(
      error = paste0("batch size ", length(inputs), " > maximum allowed batch size ", l$tei_max_batch),
      error_type = "Validation"
    ), status = 413L))
  }

  texts <- purrr::map_chr(inputs, \(x) x[[1]])
  if (any(grepl("NANTEXT", texts))) {
    return(l$send_json(res, list(error = "score is NaN", error_type = "Backend"), status = 424L))
  }

  l$send_json(res, purrr::map(texts, l$tei_logits))
})

.hf_app$get("/tei_embed/info", function(req, res) {
  l <- req$app$locals
  l$send_json(res, l$tei_info(list(embedding = list(pooling = "cls"))))
})

.hf_app$post("/tei_embed", function(req, res) {
  l <- req$app$locals
  inputs <- req$json$inputs
  if (length(inputs) > l$tei_max_batch) {
    return(l$send_json(res, list(
      error = paste0("batch size ", length(inputs), " > maximum allowed batch size ", l$tei_max_batch),
      error_type = "Validation"
    ), status = 413L))
  }
  # V1 traces the text, V2 records whether truncate was sent at the top level
  truncate <- as.numeric(isTRUE(req$json$truncate))
  l$send_json(res, purrr::map(inputs, \(x) c(nchar(x), truncate, 0)))
})

# 503 for the first two calls to /info and to the endpoint, as while scaling up from zero
.l$cold_calls <- new.env()
.l$cold_calls$info <- 0L
.l$cold_calls$post <- 0L

.hf_app$get("/tei_cold/info", function(req, res) {
  l <- req$app$locals
  l$cold_calls$info <- l$cold_calls$info + 1L
  if (l$cold_calls$info <= 2L) {
    return(l$send_json(res, list(error = "Service Unavailable"), status = 503L))
  }
  l$send_json(res, l$tei_info(list(embedding = list(pooling = "cls"))))
})

.hf_app$post("/tei_cold", function(req, res) {
  l <- req$app$locals
  l$cold_calls$post <- l$cold_calls$post + 1L
  if (l$cold_calls$post <= 1L) {
    return(l$send_json(res, list(error = "Service Unavailable"), status = 503L))
  }
  l$send_json(res, purrr::map(req$json$inputs, \(x) c(nchar(x), 1, 0)))
})

.hf_app$post("/toolkit_classify", function(req, res) {
  l <- req$app$locals
  inputs <- req$json$inputs
  texts <- unlist(inputs)

  if (any(!nzchar(texts))) {
    return(l$send_json(res, list(error = "You need to specify either text or text_target"), status = 400L))
  }
  if (any(grepl("BADTEXT", texts))) {
    return(l$send_json(res, list(error = "Bad text"), status = 400L))
  }

  result <- purrr::map(texts, \(x) list(
    list(label = "positive", score = 1 / (1 + nchar(x))),
    list(label = "negative", score = 1 - 1 / (1 + nchar(x)))
  ))
  if (!is.list(inputs)) {
    result <- result[[1]]
  }
  l$send_json(res, result)
})

.hf_app$post("/toolkit_embed", function(req, res) {
  l <- req$app$locals
  l$send_json(res, purrr::map(unlist(req$json$inputs), \(x) c(nchar(x), 0, 0)))
})

hf_server <- webfakes::local_app_process(.hf_app)
