test_that("base_request accepts inputs and creates right type of object", {
  key_name = "TEST_API_KEY"

  base_req <- expect_no_error(base_request("https://endpoint.com", key_name))
  expect_s3_class(base_req, "httr2_request")
  expect_equal("EndpointR", base_req$options$useragent)
  expect_equal("POST", base_req$method)
})

test_that("perform_requests_with_strategy accepts expected parameters", {

  expect_warning( # careful here as it doesn't throw an error, just a warning (when an error is met)
    perform_requests_with_strategy("request", indices = "hello", function(x) x),
    regexp = "must be an HTTP"
  )

  key_name = "TEST_API_KEY"
  base_req <- base_request("https://endpoint.com", key_name)
  # fail when vectors don't match lengths
  expect_error(perform_requests_with_strategy(base_req,indices = c(1, 2),
  "Can't recycle"))

})

test_that("perform_requests_with_strategy handles input parameters and returns the right data frame(s)", {
  key_name = "TEST_API_KEY"

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

  responses_seq <- expect_no_error(perform_requests_with_strategy(
    base_reqs,
    c(1, 2, 3, 4),
    tidy_embedding_response,
    concurrent_requests = 1)
  )
  responses_seq <- dplyr::bind_rows(responses_seq)
  expect_setequal(
    names(responses_seq), c("V1", "V2", "V3", "original_index", ".error", ".error_message"))

  responses_par <- expect_no_error(
    perform_requests_with_strategy(
      base_reqs,
      c(1, 2, 3, 4),
      tidy_embedding_response,
      concurrent_requests = 2)
  )

  responses_par <- dplyr::bind_rows(responses_par)

  expect_true(
    all.equal(
      responses_seq,
      responses_par
  ))


})

