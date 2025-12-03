test_that("Building messages requests works", {

  expect_error(
    ant_build_messages_request(input = c("Vector", "input")),
    "input must be a non-empty character string"
  )

  expect_error(
    ant_build_messages_request(input = "User stuff", system_prompt = c("Vector", "Prompt")), "must be a <character>"
  )


})
