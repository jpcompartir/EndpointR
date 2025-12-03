test_that("base_request accepts inputs and creates right type of object", {
  key_name = "TEST_API_KEY"

  base_req <- expect_no_error(base_request("https://endpoint.com", key_name))
  expect_s3_class(base_req, "httr2_request")
  expect_equal("EndpointR", base_req$options$useragent)
  expect_equal("POST", base_req$method)
})

test_that("perform_requests_with_strategy handles input parameters and returns untidied responses in embedding case", {
  # test uses the webfake app set up in helper-webfake.R

  key_name = "TEST_API_KEY"

  base_req <- base_request(server$url("/test_embedding"), key_name)

  base_reqs <- list(base_req, base_req, base_req, base_req)

  responses_seq <- expect_no_error(
    perform_requests_with_strategy(
      requests = base_reqs,
      concurrent_requests = 1)
  )

  expect_true(all(purrr::map(responses_seq, class) == "httr2_response"))

  # ugly, but just check everything is the expected vector from our app$post("/test", i.e. no tidying/transformation performed by perform_requests_with_strategy)
  responses_values_seq <- purrr::map(responses_seq, httr2::resp_body_json) |>
    purrr::map(unlist)

  expect_true(
    all(
      vapply(
        responses_values_seq,
        function(x) identical(x, c(0.1, 0.2, 0.3)),
        FUN.VALUE = logical(1)
      )
    )
  )

  responses_par <- expect_no_error(
    perform_requests_with_strategy(
      base_reqs,
      concurrent_requests = 2)
  )

  responses_values_par <-purrr::map(responses_par, httr2::resp_body_json) |>
    purrr::map(unlist)

  expect_true(all.equal(responses_values_seq,responses_values_par))
})

test_that("process_response handles single requests and lists mapped with indices", {

  sentiment_all_score <- list(
    list(
      list(
        label = "positive",
        score = 0.05167632
      ),
      list(
        label = "negative",
        score = 0.8648104
      ),
      list(
        label = "neutral",
        score = 0.0835133
      )
    )
  )

  # json_all_score <- jsonlite::toJSON(sentiment_all_score, auto_unbox = TRUE)
  # raw_all_score <- charToRaw(json_all_score) # one way imitate the API response

  # better way:
  mock_response <- httr2::response_json(
    body = sentiment_all_score,
    status_code = 200L,
    headers = list("Content-Type" = "application/json"))

  mock_response |>
    process_response(indices = 1, tidy_func = tidy_classification_response)

  tidied_response <- expect_no_error(mock_response |>
    tidy_classification_response())

  manual_tidy <- sentiment_all_score[[1]] |>
    purrr::map(as.data.frame) |>
    purrr::list_rbind() |>
    tidyr::pivot_wider(names_from = label, values_from = score)

  expect_true(all.equal(tidied_response, manual_tidy, tolerance = 1e-3))

  multi_sentiment_all_score <- list(mock_response, mock_response, mock_response)

  purrr::map2(
    multi_sentiment_all_score,
    c(1, 2, 3),
    ~ process_response(.x, .y, tidy_classification_response)
  ) |>
    purrr::list_rbind()

  })

test_that("process_response handles batches of inputs when passed the correct tidy function", {
  sentiment_scores <- list(
    list(
      list(label = "positive",score = 0.5865912),
      list(label = "negative",score = 0.1204563),
      list(label = "neutral",score = 0.2929525)),
    list(
      list(label = "positive",score = 0.506693),
      list(label = "negative",score = 0.110413),
      list(label = "neutral",score = 0.382894)
    ),
    list(
      list(label = "positive",score = 0.4080445),
      list(label = "negative",score = 0.1041288),
      list(label = "neutral",score = 0.4878267
      )
    )
  )

  # single batch case
  mock_batch_response <- httr2::response_json(
      body = sentiment_scores,
      status_code = 200L,
      headers = list("Content-Type" = "application/json")
      )

  single_batch <- expect_no_error(process_response(resp = mock_batch_response,
                   indices = 1:3,
                   tidy_func = tidy_batch_classification_response))
  expect_setequal(names(single_batch), c("positive", "negative", "neutral", "original_index", ".error", ".error_msg"))
  expect_equal(nrow(single_batch), 3)

  # multi-batches
  multi_batch <- list(
    sentiment_scores,
    sentiment_scores,
    sentiment_scores
  )
    multi_batch_resps <- purrr::map(multi_batch, ~ httr2::response_json(body = .x,
                                     status_code = 200L,
                                     headers = list("Content-Type" = "application/json")
                                     ))

    indices <- list(
      c(1, 2, 3),
      c(4, 5, 6),
      c(7, 8, 9)
    )

    processed_batch_results <- expect_no_error(purrr::map2(
      multi_batch_resps,
      indices,
      ~ process_response(.x, .y, tidy_func = tidy_batch_classification_response)
    ) |>
      purrr::list_rbind()
    )

    expect_equal(nrow(processed_batch_results), 9)
    expect_equal(ncol(processed_batch_results), 6)

})

test_that(".create_erorr_tibble deals with indices and messages and outputs a tibble as expected", {
  error_tib <- expect_no_error(.create_error_tibble(1:2, "hello"))

  expect_true(nrow(error_tib) ==2)

})


# error handling tests ----

test_that("base_request includes req_error to prevent auto-throwing on HTTP errors", {
 # verify that req_error policy is set on base requests
  req <- base_request("https://api.example.com", "fake_key")

 # the request should have an error policy (error_is_error) that doesn't auto-throw
  expect_true("error_is_error" %in% names(req$policies))
})

test_that(".extract_api_error extracts OpenAI-format error messages", {
 # openai format: {"error": {"message": "...", "type": "..."}}

  mock_400 <- httr2::response_json(
    status_code = 400L,
    body = list(error = list(
      message = "Invalid request: missing required parameter 'model'",
      type = "invalid_request_error"
    ))
  )

  error_msg <- .extract_api_error(mock_400)
  expect_equal(error_msg, "Invalid request: missing required parameter 'model'")

  mock_401 <- httr2::response_json(
    status_code = 401L,
    body = list(error = list(
      message = "Incorrect API key provided",
      type = "authentication_error"
    ))
  )

  error_msg <- .extract_api_error(mock_401)
  expect_equal(error_msg, "Incorrect API key provided")

  mock_429 <- httr2::response_json(
    status_code = 429L,
    body = list(error = list(
      message = "Rate limit exceeded. Please retry after 60 seconds.",
      type = "rate_limit_error"
    ))
  )

  error_msg <- .extract_api_error(mock_429)
  expect_equal(error_msg, "Rate limit exceeded. Please retry after 60 seconds.")
})

test_that(".extract_api_error extracts HuggingFace-format error messages", {
 # huggingface format: {"error": "..."}

  mock_503 <- httr2::response_json(
    status_code = 503L,
    body = list(error = "Model is currently loading, please retry in 30 seconds")
  )

  error_msg <- .extract_api_error(mock_503)
  expect_equal(error_msg, "Model is currently loading, please retry in 30 seconds")

  mock_500 <- httr2::response_json(
    status_code = 500L,
    body = list(error = "Internal server error")
  )

  error_msg <- .extract_api_error(mock_500)
  expect_equal(error_msg, "Internal server error")
})

test_that(".extract_api_error extracts Anthropic-format error messages", {
 # anthropic format: {"message": "..."}

  mock_529 <- httr2::response_json(
    status_code = 529L,
    body = list(message = "Anthropic API is temporarily overloaded")
  )

  error_msg <- .extract_api_error(mock_529)
  expect_equal(error_msg, "Anthropic API is temporarily overloaded")
})

test_that(".extract_api_error falls back to HTTP status when body parsing fails", {
 # response with non-json body or unexpected structure

  mock_502 <- httr2::response_json(
    status_code = 502L,
    body = list(unexpected_field = "something went wrong")
  )

  error_msg <- .extract_api_error(mock_502)
  expect_equal(error_msg, "HTTP 502")

  mock_403 <- httr2::response_json(
    status_code = 403L,
    body = list() # empty body
 )

  error_msg <- .extract_api_error(mock_403)
  expect_equal(error_msg, "HTTP 403")
})

test_that(".extract_api_error returns NA for successful responses", {
  mock_200 <- httr2::response_json(
    status_code = 200L,
    body = list(data = "success")
  )

  error_msg <- .extract_api_error(mock_200)
  expect_true(is.na(error_msg))
})

test_that(".extract_api_error handles non-response objects gracefully", {
 # error condition object
  err <- simpleError("Connection timed out")
  error_msg <- .extract_api_error(err)
  expect_equal(error_msg, "Connection timed out")

 # generic object with fallback
  error_msg <- .extract_api_error("not a response", fallback_message = "Unknown failure")
  expect_equal(error_msg, "Unknown failure")

 # NULL input
  error_msg <- .extract_api_error(NULL, fallback_message = "Request failed")
  expect_equal(error_msg, "Request failed")
})

test_that(".extract_api_error handles all common HTTP error status codes", {
 # test a range of common error codes
  status_codes <- c(400L, 401L, 403L, 404L, 429L, 500L, 502L, 503L, 529L)

  for (code in status_codes) {
    mock_resp <- httr2::response_json(
      status_code = code,
      body = list(error = list(message = paste("Error", code)))
    )

    error_msg <- .extract_api_error(mock_resp)
    expect_equal(error_msg, paste("Error", code), info = paste("Failed for status code", code))
  }
})

test_that("process_response handles HTTP error responses correctly", {
 # mock a 429 rate limit error
  mock_429 <- httr2::response_json(
    status_code = 429L,
    body = list(error = list(
      message = "Rate limit exceeded",
      type = "rate_limit_error"
    ))
  )

 # check warning is produced
  expect_warning(
    process_response(mock_429, indices = 1:3, tidy_func = tidy_classification_response),
    "Request failed with status 429"
  )

 # capture result for assertions
  result <- suppressWarnings(
    process_response(mock_429, indices = 1:3, tidy_func = tidy_classification_response)
  )

  expect_true(all(result$.error))
  expect_equal(result$.error_msg[1], "Rate limit exceeded")
  expect_equal(nrow(result), 3)
  expect_true("original_index" %in% names(result))

 # mock a 500 server error
  mock_500 <- httr2::response_json(
    status_code = 500L,
    body = list(error = list(message = "Internal server error"))
  )

 # check warning is produced
  expect_warning(
    process_response(mock_500, indices = c(5, 6), tidy_func = tidy_classification_response),
    "Request failed with status 500"
  )

 # capture result for assertions
  result <- suppressWarnings(
    process_response(mock_500, indices = c(5, 6), tidy_func = tidy_classification_response)
  )

  expect_true(all(result$.error))
  expect_equal(result$.error_msg[1], "Internal server error")
  expect_equal(nrow(result), 2)
})

test_that("process_response handles mixed success and error batches", {
 # this tests that we can process a mix of successful and failed responses

 # successful response
  success_body <- list(
    list(
      list(label = "positive", score = 0.9),
      list(label = "negative", score = 0.1)
    )
  )

  mock_success <- httr2::response_json(
    status_code = 200L,
    body = success_body
  )

 # error response
  mock_error <- httr2::response_json(
    status_code = 429L,
    body = list(error = list(message = "Rate limited"))
  )

  responses <- list(mock_success, mock_error)
  indices_list <- list(1, 2)

 # process both
  results <- purrr::map2(
    responses,
    indices_list,
    ~ suppressWarnings(process_response(.x, .y, tidy_func = tidy_classification_response))
  ) |>
    purrr::list_rbind()

  expect_equal(nrow(results), 2)
  expect_equal(sum(results$.error), 1) # one error
  expect_equal(sum(!results$.error), 1) # one success
})

test_that(".create_error_tibble produces correct structure with .error_msg column", {
  error_tib <- .create_error_tibble(c(1, 2, 3), "Something went wrong")

  expect_s3_class(error_tib, "tbl_df")
  expect_equal(nrow(error_tib), 3)
  expect_true(all(error_tib$.error))
  expect_true(all(error_tib$.error_msg == "Something went wrong"))
  expect_equal(error_tib$original_index, c(1, 2, 3))
  expect_setequal(names(error_tib), c("original_index", ".error", ".error_msg"))
})
