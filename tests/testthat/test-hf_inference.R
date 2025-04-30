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

  withr::with_envvar(
    c("TEST_API_KEY" = "gibberish"),
    {
      expect_error(
        hf_build_request(
          input = list("hello", "number 2"),
          endpoint_url =endpoint_url,
          key_name = "TEST_API_KEY"))

      test_req <- expect_no_error(hf_build_request(input = "hello",
                                                   endpoint_url = endpoint_url,
                                                   key_name = "TEST_API_KEY"))
    })
  expect_true(test_req$body$data$inputs == "hello")

})

test_that("hf_build_request_batch handles batches",{
  endpoint_url <- "https://gibberish.com"
  texts <- list("batch text1", "batch text2")
  key_name <- "TEST_API_KEY"

  withr::with_envvar(
    c("TEST_API_KEY" = "gibberish"),
    {
      test_batch_req <-
        expect_no_error(
          hf_build_request_batch(
            inputs = texts,
            endpoint_url = endpoint_url,
            key_name = key_name))
    })

  expect_length(test_batch_req$body$data$inputs, 2)
})


test_that("hf_build_request_df's arguments are functioning", {
  texts <- paste0("text", 1:10)
  ids <- seq(1:length(texts))
  test_df <- data.frame(id = ids, text = texts)
  endpoint_url <- "https://gibberish.com"
  key_name <- "TEST_API_KEY"

  withr::with_envvar( # preferable to mockery::stub
    c("TEST_API_KEY" = "gibberish"),
    {
      result <-
        expect_no_error(
          hf_build_request_df(
            df = test_df,
            text_var = text,
            id_var = id,
            endpoint_url = endpoint_url,
            key_name = key_name,
            max_retries = 10,
            timeout = 18))
      })

  expect_true(".request" %in% names(result))
  single_request <- result$.request[[1]]
  expect_s3_class(single_request, "httr2_request")
  expect_equal(single_request$options$timeout_ms, 18000)

  expect_equal(single_request$policies$retry_max_tries, 10)
})
