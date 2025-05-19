test_that("tidy_classification_response handles a single response with $labels and $score in the body", {

  sentiment_scores <- list(
    list(
      list(label = "positive", score = 0.01),
      list(label = "negative", score = 0.98),
      list(label = "neutral", score = 0.01)
      )
    )

  tidied_response <- expect_no_error(
    response_json(
      status_code = 200L,
      body = sentiment_scores
    ) |>
      resp_body_json() |>
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
    response_json(
      status_code = 200L,
      body = spam_scores,
    ) |>
      resp_body_json() |>
      tidy_classification_response()
  )

  expect_equal(nrow(tidied_spam_response), 1)
  expect_setequal(names(tidied_spam_response), c("spam", "not_spam"))
})

test_that("tidy_batch_classification_response handes a batch response with multiple $label and multiple $score in the body",{

} )

