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


  req_gptxx <- oai_build_completions_request(input = "test2",
                                             model = "GPTXX", # invalid model obvs but does thea rg work is the Q
                                             timeout = 10,
                                             max_retries = 10,
                                             max_tokens = 20,
                                             temperature = 1.0)
  expect_equal(req_gptxx$body$data$model, "GPTXX")
  expect_equal(req_gptxx$options$timeout_ms, 10000)
  expect_equal(req_gptxx$body$data$messages[[1]][["content"]], "test2")
  expect_equal(req_gptxx$policies$retry_max_tries, 10)
  expect_equal(req_gptxx$body$data$temperature, 1)
  expect_equal(req_gptxx$body$data$max_tokens, 20)
})

test_that("oai_build_completions_request takes a json_schema and adds it to the response body, and accepts a list as a schema", {
  test_schema <- create_json_schema(
    name = "test_json_schema",
    schema = schema_object(name = "hello"),
    strict = TRUE
  )

  request_w_json_schema <- expect_no_error(
    EndpointR::oai_build_completions_request(
      input = "hello",
      schema = test_schema
  ))

  expect_false(request_w_json_schema$body$data$response_format$json_schema$schema$additionalProperties)

  names(request_w_json_schema$body$data)


  list_schema <- list(
    name = "list_schema",
    schema = list(
      type = "object",
      properties = list("product"),
      additionalProperties = FALSE
    ),
    strict = TRUE
  )

  request_w_list_schema <- expect_no_error(
    EndpointR::oai_build_completions_request(
      input = "hello",
      schema = list_schema
    )
  )
  expect_false(request_w_list_schema$body$data$response_format$schema$additionalProperties)
})

test_that("oai_build_completions_request adds endpointr_id to headers if asked to",{
  base_req <- oai_build_completions_request(input = "hello")

  expect_false("endpointr_id" %in% names(base_req$headers))

  req_with_id <- oai_build_completions_request(input = "hello",
                                               endpointr_id = "id_101")

  expect_true("endpointr_id" %in% names(req_with_id$headers))
})

test_that("oai_build_completions_request_list receives a list, and returns a list of requests", {

  # we actually didn't need this function because we can just build the batch requests from completions_request, so feels cute but may delete later

  list_req <- expect_no_error(
    oai_build_completions_request_list(c("hello", "goodbye"))
  )

  expect_true(inherits(list_req, "list"))

  expect_true(lapply(list_req, class) |>
    unlist() |>
    unique() == "httr2_request")

})

test_that("oai_build_completions_request_list properly deals with endpointr_id",{

  # TODO:
  # no id case

  # with ids case and check order is correct.
})

test_that("oai_complete_text takes a single text and returns the response, plus deals with schemas and validation", {

  # warning, this a unit test not an integration test. An integration test requires actually sending the request to the OpenAI API rather than mocking.

  test_url <-  server$url("/test_oai_schemaless")

  withr::with_envvar(
    c("OPENAI_API_KEY" = "gibberish"),
    {
     response <- expect_no_error(oai_complete_text(
       text = "Give me a helpful response to my question",
       endpoint_url = test_url
       ))
    }
  )
  expect_true(grepl(x = response[[1]], pattern = "This is a helpful"))

})

test_that("oai_complete_text handles a schema appropriately", {

  test_url <-  server$url("/test_oai_sentiment")
  sentiment_schema <- create_json_schema(
    name = "sentiment_schema",
    schema = schema_object(
    sentiment = schema_enum(values = c("positive", "negative", "neutral"), type = "string"),
    confidence = schema_number(minimum = 0, maximum = 1),
    is_spam = schema_boolean()
  ))

  # mismatch in the mocked API response and what we actually expect back, TBD fix
  withr::with_envvar(
    c("OPENAI_API_KEY" = "gibberish"),
    {
      schema_response <- expect_no_error(
        oai_complete_text(
          text = "What a remarkable achievement",
          system_prompt = "classify the sentiment of this text: ",
          endpoint_url = test_url,
          schema = sentiment_schema,
          tidy = FALSE
        ))
    }
  )

  expect_no_error(validate_response(sentiment_schema, schema_response[[1]]))

})

test_that("oai_complete_df takes single row, multi-row data frames as inputs", {
  expect_error(oai_complete_df("hello"),
               regexp = "df must be")

  review_df <- get_review_df()

  server$start()
  endpoint_url <- server$url("/test_complete_df_review")

  withr::with_envvar(
    c("OPENAI_API_KEY" = "gibberish"),
    successful_response <- expect_no_error(
      oai_complete_df(review_df,
                      review_text,
                      id,
                      endpoint_url = endpoint_url,
                      concurrent_requests = 1,
                      max_retries = 1)

      )
    )

  expect_setequal(names(successful_response),
                  c("id", "review_text", "status", "content", ".error_msg", ".error"))
  expect_setequal(unique(successful_response$content), "positive")

  withr::with_envvar(
    c("OPENAI_API_KEY" = "gibberish"),
    expect_message(object =
      oai_complete_df(review_df,
                      review_text,
                      id,
                      endpoint_url = endpoint_url,
                      concurrent_requests = 1,
                      max_retries = 1),

      regexp = "Performing 5 requests sequentially"
    )
  )

  # check we at least get the status update of concurrency - even if we can't properly test it here.
  withr::with_envvar(
    c("OPENAI_API_KEY" = "gibberish"),
    expect_message(object =
                     oai_complete_df(review_df,
                                     review_text,
                                     id,
                                     endpoint_url = endpoint_url,
                                     concurrent_requests = 5,
                                     max_retries = 1),

                   regexp = "with 5 concurrent requests"
    )
  )
  })


test_that("oai_complete_df takes a schema as input and validates", {

  sentiment_schema <- create_json_schema(
    name = "sentiment_test",
    schema = schema_object(
      sentiment = schema_enum(
        values = c("positive", "negative", "neutral"),
        description = "Sentiment classification for the document",
        type = "string"
      ),
      required = list("sentiment"),
      additional_properties = FALSE
    )
  )

  invalid_sentiment_schema <- create_json_schema(
    name = "sentiment_test",
    schema = schema_object(
      sentiment = schema_enum(
        values = c("positif", "negatif", "nootral"),
        description = "Sentiment classification for the document",
        type = "string"
      ),
      required = list("sentiment"),
      additional_properties = FALSE
    )
  )

  review_df <- get_review_df()

  # server$start()
  endpoint_url <- server$url("/test_complete_df_schema")

  withr::with_envvar(
    c("OPENAI_API_KEY" = "gibberish"),
    successful_response <- expect_no_error(
      oai_complete_df(review_df,
                      review_text,
                      id,
                      endpoint_url = endpoint_url,
                      concurrent_requests = 1,
                      max_retries = 1,
                      schema = sentiment_schema
                      )

    )
  )


  # this does error as expected.
  withr::with_envvar(
    c("OPENAI_API_KEY" = "gibberish"),
    not_validated_response <- expect_warning(
      oai_complete_df(review_df,
                      review_text,
                      id,
                      endpoint_url = endpoint_url,
                      concurrent_requests = 1,
                      max_retries = 1,
                      schema = invalid_sentiment_schema
      ), regexp = "5 responses failed schema validation"
    )
  )



})


test_that("oai_complete_df handles mixed validation success/failure", {

  review_df <- get_review_df()


  sentiment_schema <- create_json_schema(
    name = "sentiment_test_mixed",
    schema = schema_object(
      sentiment = schema_enum(
        values = c("positive", "negative", "neutral"),
        description = "Sentiment classification for the document",
        type = "string"
        )
    )
  )

  #  reset counter JIC - should get torn down with the defer call in testthat setup
  if (exists(".request_counter")) rm(.request_counter, envir = .GlobalEnv)

  endpoint_url <- server$url("/test_complete_df_mixed")

  # should get alternating successes and failures
  withr::with_envvar(
    c("OPENAI_API_KEY" = "gibberish"),
    {
      expect_warning(
        results <- oai_complete_df(
          review_df,
          review_text,
          id,
          endpoint_url = endpoint_url,
          concurrent_requests = 1,
          max_retries = 1,
          schema = sentiment_schema
        ),
        regexp = "responses failed schema"
      )
    }
  )

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 5)

  expect_true("content" %in% names(results))
  expect_false("sentiment" %in% names(results))


  expect_true(all(grepl("sentiment", results$content)))
})
