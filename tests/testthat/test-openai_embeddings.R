test_that("tidy_oai_embedding handles single and batches",{

  mock_single <- list(
    list(
      object = "embedding",
      index = 0,
      embedding = list(0.1, 0.2, 0.3)
    )
  )

  mock_batch <- list(
    list(
      object = "embedding",
      index = 0,
      embedding = list(0.1, 0.2, 0.3)
    ),
    list(
      object = "embedding",
      index = 1,
      embedding = list(0.2, 0.3, 0.4)
    )
  )

  tidied_single <- tidy_oai_embedding(mock_single)
  expect_true(inherits(tidied_single, "data.frame"))
  expect_equal(nrow(tidied_single), 1)
  expect_setequal(names(tidied_single), c("oai_index", "V1", "V2", "V3"))


  tidied_batch <- tidy_oai_embedding(mock_batch)
  expect_equal(nrow(tidied_batch), 2)
  expect_setequal(names(tidied_single), names(tidied_batch))
})

test_that("oai_embed_batch handles batches properly",{

  expect_error(
    oai_embed_batch(c(2, 3)),
    regexp = "Each individual text must")


  # future test cases when we mock OpenAI API
  # oai_embed_batch(c(2, 3)) # error
  # oai_embed_batch(c("hello", "goodbye")) # fine
  # oai_embed_batch(c("hello", "")) # error empty string

})
