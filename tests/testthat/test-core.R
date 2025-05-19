test_that("base_request accepts inputs and creates right type of object", {
  key_name = "TEST_API_KEY"

  base_req <- expect_no_error(base_request("https://endpoint.com", key_name))
  expect_s3_class(base_req, "httr2_request")
  expect_equal("EndpointR", base_req$options$useragent)
  expect_equal("POST", base_req$method)
})

test_that("perform_requests_with_strategy handles input parameters and returns untidied responses in embedding case", {
  key_name = "TEST_API_KEY"

  # we don't want to test live connections to APIs etc. so we use a fake API that returns the type of response we want.
  app <- webfakes::new_app()
  app$post("/test",
           function(req, res) {
             res$
               set_status(200L)$
               send_json(c(0.1, 0.2, 0.3))
           })
  server <- webfakes::new_app_process(app)
  base_req <- base_request(server$url("/test"), key_name)

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
  expect_setequal(names(single_batch), c("positive", "negative", "neutral", "original_index", ".error", ".error_message"))
  expect_equal(nrow(single_batch), 3)

  # multi-batches
  multi_batch <- list(
    sentiment_scores,
    sentiment_scores,
    sentiment_scores
  )
    multi_batch_resps <- map(multi_batch, ~ response_json(body = .x,
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
