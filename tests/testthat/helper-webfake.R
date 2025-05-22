withr::deferred_run()
withr::local_envvar(HF_TEST_API_KEY = "fake-key")

.app <- webfakes::new_app()

.app$post("/test_embedding", function(req, res) {
  res$
    set_status(200L)$
    send_json(c(0.1, 0.2, 0.3))
})

.app$post("/test_single_sentiment", function(req, res) {
  response_data <- list(
    list(
      list(label = "positive", score = 0.9),
      list(label = "negative", score = 0.05),
      list(label = "neutral", score = 0.05)
    )
  )

  res$set_header("Content-Type", "application/json")
  json_string <- jsonlite::toJSON(response_data, auto_unbox = TRUE)
  res$send(json_string)
})

.app$post("/test_list_sentiment", function(req, res){
  response_data <- list(
    list(
      list(label = "positive", score = 0.5866),
      list(label = "negative", score = 0.1205),
      list(label = "neutral", score = 0.293)
    ),
    list(
      list(label = "positive", score = 0.5067),
      list(label = "negative", score = 0.1104),
      list(label = "neutral", score = 0.3829)
    ),
    list(
      list(label = "positive", score = 0.65),
      list(label = "negative", score = 0.00),
      list(label = "neutral", score = 0.35)
    ),
    list(
      list(label = "positive", score = 0.65),
      list(label = "negative", score = 0.00),
      list(label = "neutral", score = 0.35)
    )
  )

  res$set_header("Content-Type", "application/json")

  json_string <- jsonlite::toJSON(response_data, auto_unbox = TRUE)
  res$send(json_string)
})

.app$post("/test_batch_embedding", function(req, res) {
  # create embeddings that match the format expected by tidy_embedding_response
  # for a batch of 2 texts, return 2 embedding vectors
  embeddings <- list(
    c(0.1, 0.2, 0.3),
    c(0.2, 0.4, 0.6)
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send_json(embeddings)
})


.app$post("/test_error", function(req, res) {
  res$set_status(500L)
  res$set_header("Content-Type", "application/json")
  res$send('{"error": "Test error response"}')
})




server <- webfakes::local_app_process(.app)


# example test uses:
# hf_classify_text(
#   "this is a test case",
#   endpoint_url = server$url("/test_single_sentiment"),
#   key_name = "HF_TEST_API_KEY"
# )
#
# texts <- c("This is a test text", "and this is another", "yes, another", "oh, another.")
#
# test_df <- data.frame(
#   id = c(1,2,3,4),
#   text = texts,
#   stringsAsFactors = FALSE
# )
#
#
# hf_classify_df(
#   df = test_df,
#   text_var = text,
#   id_var = id,
#   endpoint_url = server$url("/test_list_sentiment"),
#   key_name = "HF_TEST_API_KEY",
#   batch_size = 4
# )
#
# hf_classify_df(
#   df = test_df,
#   text_var = text,
#   id_var = id,
#   endpoint_url = server$url("/test_list_sentiment"),
#   key_name = "HF_TEST_API_KEY",
#   batch_size = 4
# )
#
#
# hf_classify_batch(
#   texts = texts,
#   endpoint_url = server$url("/test_list_sentiment"),
#   key_name = "HF_TEST_API_KEY",
#   batch_size = 2
# )
