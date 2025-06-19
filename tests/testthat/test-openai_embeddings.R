test_that("tidy_oai_embedding handles single and batches",{

  mock_single <- list(
    list(
      object = "embedding",
      index = 0,
      embedding = list(0.1, 0.2, 0.3, 0.4, 0.5)
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
  tidied_batch <- tidy_oai_embedding(mock_batch)


})

