test_that("validate_hf_endpoint errors if given invalid URL", {

  mockery::stub(validate_hf_endpoint, "get_api_key", "HF_TEST_API_KEY")
  expect_error(
    validate_hf_endpoint(
      endpoint_url = "xxx_yy.com",
      key_name = "HF_TEST_API_KEY"
      ),
    regexp = "Cannot connect to Hugging")

  # testing strategy is a bit difficult as we're working with endpoints that won't be up etc. so we don't *really* want to do check for success, at least not when CI/CD runs. Need to think more on this, they may just be local tests for most things - which is not ideal.

# Don't love the idea of stubs or mocks etc. as they often give a false sense of security - which can be more damaging than beneficial.


})

# test_that("tidy_embedding_response tidies single requests and batches", {
#
# })
