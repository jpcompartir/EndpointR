test_that("build_request has the correct default request body", {
  endpoint_url <- "gibberish.com"
  api_key <- "gibberish"

  req <- base_request(endpoint_url, api_key)
  expect_true(req$method == "POST")
  expect_true(req$headers$`Content-Type` == "application/json")
  expect_true(req$options$useragent == "EndpointR")

})

test_that("hf_build_request does not try to handle batches", {
  # what's the best way to handle this, shouold I spin up a tmp .Renviron file and put the a key in, or just use HF_TEST_API_KEY? I don't kike the latter as it won't be obvious to anyone else who develops and it will be extra work in CI/CD
  endpoint_url <- "https://gibberish.com"

  mockery::stub(hf_build_request, "get_api_key", "TEST_API_KEY") # as hf_build_request calls get_api_key, we need to make sure we return a string of the right form

  expect_error(hf_build_request(input = list("hello", "number 2"),
                   endpoint_url =endpoint_url,
                   key_name = "TEST_API_KEY"))

  test_req <- expect_no_error(hf_build_request(input = "hello",
                                   endpoint_url = endpoint_url,
                                   key_name = "TEST_API_KEY"))

  expect_true(test_req$body$data$inputs == "hello")

})

test_that("hf_build_request_batch handles batches",{
  endpoint_url <- "https://gibberish.com"
  mockery::stub(hf_build_request_batch, "get_api_key", "TEST_API_KEY")
  texts <- list("batch text1", "batch text2")




  test_batch_req <- expect_no_error(hf_build_request_batch(inputs = texts, endpoint_url = endpoint_url, key_name = "TEST_API_KEY"))



})
