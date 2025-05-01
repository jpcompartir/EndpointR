test_that("validate_hf_endpoint errors if given invalid URL", {

  key_name = "TEST_API_KEY"

  withr::with_envvar(
    c("TEST_API_KEY" = "gibberish"),{
    expect_error(
      validate_hf_endpoint(
        endpoint_url = "xxx_yy.com",
        key_name = key_name
      ),
      regexp = "Cannot connect to Hugging")
  })

  # testing strategy is a bit difficult as we're working with endpoints that won't be up etc. so we don't *really* want to do check for success, at least not when CI/CD runs. Need to think more on this, they may just be local tests for most things - which is not ideal.

})

# following on from comment in 'valiate_hf_endpoint' test, tidy functions are checking for httr2 response, so we need to mimic a httr2 response that returns embeddings. Did this in the start of package development with webfakes, but httr2 actually has objects for this - `response`, `response_json` and some mocking objects, so we can keep it quite simple.

test_that("tidy_embedding_response tidies single requests and batches", {
  single_resp <- httr2::response_json(
    status_code = 200,
    headers = list("content-type" = "application/json"),
    body = list(c(0.01, 0.02, 0.03, 0.04, 0.05))
    )

  tidied_resp <- expect_no_error(tidy_embedding_response(single_resp))
  expect_equal(nrow(tidied_resp), 1)
  expect_equal(ncol(tidied_resp), 5)

  batch_resp <- httr2::response_json(
    status_code = 200,
    headers = list("content-type" = "application/json"),
    body = list(
      c(0.1, 0.2, 0.3, 0.4, 0.5),
      c(0.6, 0.7, 0.8, 0.9, 1.0),
      c(1.1, 1.2, 1.3, 1.4, 1.5)
    )
  )

  tidied_batch_resp <- expect_no_error(tidy_embedding_response(batch_resp))
  expect_equal(nrow(tidied_batch_resp), 3)
  expect_equal(ncol(tidied_batch_resp), 5)
})
