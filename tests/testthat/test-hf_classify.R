test_that("tidy_classification_response handles a single response with $labels and $score in the body", {

  sentiment_scores <- list(
    list(
      list(label = "positive", score = 0.01),
      list(label = "negative", score = 0.98),
      list(label = "neutral", score = 0.01)
      )
    )

  tidied_response <- expect_no_error(
    httr2::response_json(
      status_code = 200L,
      body = sentiment_scores
    ) |>
      httr2::resp_body_json() |>
      tidy_classification_response()
  )

  expect_setequal(names(tidied_response), c("positive", "negative", "neutral"))


  spam_scores <- list(
    list(
      list(label = "spam", score = 0.55),
      list(label = "not_spam", score = 0.45)
    )
  )

  tidied_spam_response <- expect_no_error(
    httr2::response_json(
      status_code = 200L,
      body = spam_scores,
    ) |>
      httr2::resp_body_json() |>
      tidy_classification_response()
  )

  expect_equal(nrow(tidied_spam_response), 1)
  expect_setequal(names(tidied_spam_response), c("spam", "not_spam"))
})

test_that("tidy_batch_classification_response handes a batch response with multiple $label and multiple $score in the body",{

  batch_spam_scores <- list(
    list(
      list(label = "spam", score = 0.58),
      list(label = "not_spam", score = 0.42)
    ),
    list(
      list(label = "spam", score = 0.58),
      list(label = "not_spam", score = 0.42)
    )
  )

  # single batch case
  mock_batch_response <- httr2::response_json(
    body = batch_spam_scores,
    status_code = 200L,
    headers = list("Content-Type" = "application/json")
  )

  tidied_batch_response <- expect_no_error(tidy_batch_classification_response(mock_batch_response))

  expect_setequal(names(tidied_batch_response), c("spam", "not_spam"))
  expect_equal(nrow(tidied_batch_response), 2)

})

test_that("hf_classify_text takes a text and returns a tidied classification response", {

sentiment_scores <- list(
  list(
    list(label = "positive", score = 0.05167632),
    list(label = "negative", score = 0.8648104),
    list(label = "neutral",  score = 0.0835133)
  ))


  mock_json_response_body <- jsonlite::toJSON(sentiment_scores,
                                              auto_unbox = FALSE)

  app <- webfakes::new_app()
  app$data$response_body <- sentiment_scores

  app$post("/", function(req, res) {
    res$set_status(200L)$
      set_header("Content-Type", "application/json")$
      send_json(app$data$response_body)
  })

  server <- webfakes::new_app_process(app)

  withr::local_envvar(HF_TEST_API_KEY = "fake-key")

  base_req <- hf_build_request(
    input = "classify me",
    endpoint_url = server$url("/"),
    key_name = "HF_TEST_API_KEY",
    parameters = list()
  )

  hf_classify_text(
    "classify me",
    endpoint_url = server$url("/"),
    key_name = "HF_TEST_API_KEY",
    parameters = list(),
    tidy = FALSE
  ) |>
    httr2::resp_body_json()

})
