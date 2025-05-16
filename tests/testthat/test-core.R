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
