test_that("validate_hf_endpoint errors if given invalid URL", {

  expect_error(
    validate_hf_endpoint(
      endpoint_url = "xxx_yy.com",
      key_name = mockery::stub("HF_TEST_API_KEY")),
    regexp = "Cannot connect to Hugging")

  # testing strategy is a bit difficult as we're working with endpoints that won't be up etc. so we don't *really* want to do check for success, at least not when CI/CD runs


})
