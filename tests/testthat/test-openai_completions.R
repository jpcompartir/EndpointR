# testing plan -
# build_completions_request
  # - []
  # - []
  # - []
# build_completions_request_list
  # - []
  # - []
  # - []
# oai_complete_text
  #  [ ] endpoint faked with POST endpoints added in tests/helper-webfake.R added
# oai_complete_df
  #  [ ] endpoint faked with POST endpoints added in tests/helper-webfake.R added

# Helper funcs:
#.extract_response_fields
# .extract_oai_completions_content

test_that("oai_build_completions_request validates inputs and generates valid requests", {

  expect_error(oai_build_completions_request(),
               regexp = 'input')

  # default args check:
  req <- expect_no_error(oai_build_completions_request(input = "test"))
  expect_equal(req$url, "https://api.openai.com/v1/chat/completions")
  expect_equal(req$policies$retry_max_tries, 5)
  # str(req)
  expect_equal(req$options$timeout_ms, 20000) # 20s
  expect_equal(req$method, "POST")
  expect_equal(req$body$data$model, "gpt-4.1-nano")
  expect_equal(req$body$data$messages[[1]][["content"]], "test")

  str(req_gptxx)
  req_gptxx <- oai_build_completions_request(input = "test2", model = "GPTXX",
                                             timeout = 10,
                                             max_retries = 10,
                                             max_tokens = 20,
                                             temperature = 1.0) # invalid but does param work?
  expect_equal(req_gptxx$body$data$model, "GPTXX")
  expect_equal(req_gptxx$options$timeout_ms, 10000)
  expect_equal(req_gptxx$body$data$messages[[1]][["content"]], "test2")
  expect_equal(req_gptxx$policies$retry_max_tries, 10)
  expect_equal(req_gptxx$body$data$temperature, 1)
  expect_equal(req_gptxx$body$data$max_tokens, 20)
})
