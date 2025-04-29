library(readr)
library(tidyr)
library(dplyr)
library(purrr)
benchmark_params <- crossing(
  batch_size = c(1, 4, 8, 16, 32),
  concurrent_requests = c(5, 10, 15, 20)
)

trust <- readr::read_csv("~/data/trust/trust_slice_spam_classification.csv") |>
  select(text) |>
  mutate(id = row_number()) |>
  filter(!is.na(text), text != "")

chunk_size <- 2000
total_chunks <- 20

trust_chunks <- trust |>
  mutate(chunk_id = ceiling(id / chunk_size)) |>
  group_split(chunk_id) |>
  head(total_chunks)

benchmark_params <- crossing(
  batch_size = c(1, 4, 8, 16, 32),
  concurrent_requests = c(1, 5, 10, 20)
) |>
  mutate(chunk_index = row_number() %% total_chunks + 1)

benchmark_results <- benchmark_params |>
  mutate(result = pmap(list(batch_size, concurrent_requests, chunk_index), function(bs, cr, ci) {
    current_chunk <- trust_chunks[[ci]]

    start_time <- Sys.time()

    res <- try(hf_embed_df(
      df = current_chunk,
      text_var = text,
      id_var = id,
      endpoint_url = endpoint_url,
      key_name = "HF_TEST_API_KEY",
      batch_size = bs,
      concurrent_requests = cr,
      progress = TRUE
    ), silent = TRUE)

    elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
    success <- !inherits(res, "try-error")
    rows_processed <- if(success) nrow(current_chunk) else 0

    list(
      elapsed_time = elapsed_time,
      success = success,
      rows_processed = rows_processed,
      throughput = if(success) rows_processed / elapsed_time else 0
    )
  })) |>
  unnest_wider(result)

benchmark_results |> readr::write_csv("dev_docs/batch_concurrent_benchmark.csv")
