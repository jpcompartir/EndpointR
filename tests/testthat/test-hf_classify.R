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


  # there's something I don't understand with webfakes, where these tests will pass when run in the `test_that()` function call, but the code will not *always* run when outside of that context. There will sometimes by a HTTP 500 Internal Server Error,which I think means the app is dying. I think to test individually we need to use $listen() rather than $local_app_process or $new_app_process but I don't fully understand why.

  json_string <- '[[{"label":"positive","score":0.05167632},{"label":"negative","score":0.8648104},{"label":"neutral","score":0.0835133}]]'

  app <- webfakes::new_app()

  app$post("/", function(req, res) {
    res$set_status(200L)
    res$set_header("Content-Type", "application/json")
    res$send(json_string)
  })

  server <- webfakes::local_app_process(app)

  withr::local_envvar(HF_TEST_API_KEY = "fake-key")

  direct_response <- httr2::request(server$url("/")) |>
    httr2::req_method("POST") |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(direct_response), 200)
  expect_equal(httr2::resp_body_string(direct_response), json_string)

  base_req <- hf_build_request(
    input = "classify me",
    endpoint_url = server$url("/"),
    key_name = "HF_TEST_API_KEY",
    parameters = list()
  )

  response <- httr2::req_perform(base_req)
  expect_equal(httr2::resp_status(response), 200)

  raw_result <- hf_classify_text(
    "classify me",
    endpoint_url = server$url("/"),
    key_name = "HF_TEST_API_KEY",
    parameters = list(),
    tidy = FALSE
  )

  expect_s3_class(raw_result, "httr2_response")
  expect_equal(raw_result$status_code, 200)

  tidy_result <- hf_classify_text(
    "classify me",
    endpoint_url = server$url("/"),
    key_name = "HF_TEST_API_KEY",
    parameters = list(),
    tidy = TRUE
  )

  print(tidy_result) # add this so I can see it outside of the test_that context, and verify for myself there's nothing funky with the tests passing when they shouldn't.

  expect_s3_class(tidy_result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(tidy_result), 3)
  expect_true(all(c("positive", "negative", "neutral") %in% names(tidy_result)))
  expect_equal(tidy_result$positive, 0.05167632, tolerance = 1e-6)
  expect_equal(tidy_result$negative, 0.8648104, tolerance = 1e-6)
  expect_equal(tidy_result$neutral, 0.0835133, tolerance = 1e-6)
})
