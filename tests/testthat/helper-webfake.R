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

.app$post("/test_df_embedding", function(req, res) {
  # create embeddings that match the format expected by hf_embed_df
  # this endpoint should behave the same as test_batch_embedding since hf_embed_df uses hf_embed_batch internally
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


.app$post("/test_oai_schemaless", function(req, res) {
  response_data <- list(
    choices = list(
      list(
        message = list(
          content = "This is a helpful response to your question."
        )
      )
    )
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send_json(response_data)
})

.app$post("/test_complete_df_review", function(req, res) {

  # it's easier, at leasr for now to just have one response (positive), despite that not really echoing what the review_df has it. It's for convenience, for now, until later figuring out how to properly mock multiple responses efficiently.

  response_data <- list(
    choices = list(
      list(
        index = 0,
        message = list(
          role = "assistant",
          content = "positive"
        ),
        finish_reason = "stop"
      )
    )
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send(jsonlite::toJSON(response_data, auto_unbox = TRUE))
})


.app$post("/test_complete_df_schema", function(req, res) {
  response_data <- list(
    choices = list(
      list(
        index = 0,
        message = list(
          role = "assistant",
          content = '{"sentiment": "positive"}'
        ),
        finish_reason = "stop"
      )
    )
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send(jsonlite::toJSON(response_data, auto_unbox = TRUE))

})

.app$post("/test_complete_df_mixed", function(req, res) {
  if (!exists(".request_counter")) .request_counter <<- 0
  .request_counter <<- .request_counter + 1

  # alternate between valid "positive" and invalid "positif"
  if (.request_counter %% 2 == 1) {
    # Valid response
    response_data <- list(
      choices = list(
        list(
          index = 0,
          message = list(
            role = "assistant",
            content = '{"sentiment": "positive"}'
          ),
          finish_reason = "stop"
        )
      )
    )
  } else {

    response_data <- list(
      choices = list(
        list(
          index = 0,
          message = list(
            role = "assistant",
            content = '{"sentiment": "positif"}'
          ),
          finish_reason = "stop"
        )
      )
    )
  }

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send(jsonlite::toJSON(response_data, auto_unbox = TRUE))
})


.app$post("/test_oai_sentiment", function(req, res) {
  sentiment_response <- list(
    sentiment = "positive",
    confidence = 0.85,
    is_spam = FALSE
  )

  response_data <- list(
    choices = list(
      list(
        message = list(
          content = jsonlite::toJSON(sentiment_response, auto_unbox = TRUE)
        )
      )
    )
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send_json(response_data)
})


# Anthropic Messages API mock endpoints
.app$post("/test_ant_schemaless", function(req, res) {
  response_data <- list(
    content = list(
      list(
        type = "text",
        text = "This is a helpful response from Claude."
      )
    ),
    stop_reason = "end_turn"
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send_json(response_data)
})

.app$post("/test_ant_sentiment", function(req, res) {
  sentiment_response <- list(
    sentiment = "positive",
    confidence = 0.85
  )

  response_data <- list(
    content = list(
      list(
        type = "text",
        text = jsonlite::toJSON(sentiment_response, auto_unbox = TRUE)
      )
    ),
    stop_reason = "end_turn"
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send_json(response_data)
})

.app$post("/test_ant_complete_df_review", function(req, res) {
  response_data <- list(
    content = list(
      list(
        type = "text",
        text = "positive"
      )
    ),
    stop_reason = "end_turn"
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send(jsonlite::toJSON(response_data, auto_unbox = TRUE))
})

.app$post("/test_ant_complete_df_schema", function(req, res) {
  response_data <- list(
    content = list(
      list(
        type = "text",
        text = '{"sentiment": "positive"}'
      )
    ),
    stop_reason = "end_turn"
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send(jsonlite::toJSON(response_data, auto_unbox = TRUE))
})

# Anthropic Batches API mock endpoints
.app$post("/test_ant_batch_create", function(req, res) {
  response_data <- list(
    id = "msgbatch_test123",
    type = "message_batch",
    processing_status = "in_progress",
    request_counts = list(
      processing = 2L,
      succeeded = 0L,
      errored = 0L,
      canceled = 0L,
      expired = 0L
    ),
    created_at = "2025-01-01T00:00:00Z",
    ended_at = NULL,
    results_url = NULL
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send(jsonlite::toJSON(response_data, auto_unbox = TRUE, null = "null"))
})

.app$get("/test_ant_batch_status_ended/:batch_id", function(req, res) {
  response_data <- list(
    id = req$params$batch_id,
    type = "message_batch",
    processing_status = "ended",
    request_counts = list(
      processing = 0L,
      succeeded = 2L,
      errored = 0L,
      canceled = 0L,
      expired = 0L
    ),
    created_at = "2025-01-01T00:00:00Z",
    ended_at = "2025-01-01T01:00:00Z",
    results_url = NULL
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send(jsonlite::toJSON(response_data, auto_unbox = TRUE, null = "null"))
})

# ant_batch_status uses endpoint_url/batch_id so we need a wildcard route
.app$get("/test_ant_batch_status_in_progress/:batch_id", function(req, res) {
  response_data <- list(
    id = req$params$batch_id,
    type = "message_batch",
    processing_status = "in_progress",
    request_counts = list(
      processing = 1L,
      succeeded = 1L,
      errored = 0L,
      canceled = 0L,
      expired = 0L
    ),
    created_at = "2025-01-01T00:00:00Z",
    ended_at = NULL,
    results_url = NULL
  )

  res$
    set_status(200L)$
    set_header("Content-Type", "application/json")$
    send(jsonlite::toJSON(response_data, auto_unbox = TRUE, null = "null"))
})

.app$get("/test_ant_batch_results", function(req, res) {
  # JSONL with one succeeded and one errored result
  line1 <- jsonlite::toJSON(list(
    custom_id = "t1",
    result = list(
      type = "succeeded",
      message = list(
        role = "assistant",
        content = list(list(type = "text", text = "Hello response")),
        stop_reason = "end_turn",
        usage = list(input_tokens = 10L, output_tokens = 5L)
      )
    )
  ), auto_unbox = TRUE)

  line2 <- jsonlite::toJSON(list(
    custom_id = "t2",
    result = list(
      type = "errored",
      error = list(type = "invalid_request", message = "Invalid request")
    )
  ), auto_unbox = TRUE)

  jsonl_content <- paste0(line1, "\n", line2, "\n")

  res$
    set_status(200L)$
    set_header("Content-Type", "application/jsonl")$
    send(jsonl_content)
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
