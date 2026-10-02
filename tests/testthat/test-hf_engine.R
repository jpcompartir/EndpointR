# engine detection ----

test_that("hf_detect_engine returns tei for TEI endpoints and toolkit for others", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")

  tei <- hf_detect_engine(hf_server$url("/tei_classify"), "HF_TEST_API_KEY")
  expect_equal(tei$engine, "tei")
  expect_equal(tei$info$max_client_batch_size, 4)

  toolkit <- hf_detect_engine(hf_server$url("/toolkit_classify"), "HF_TEST_API_KEY")
  expect_equal(toolkit$engine, "toolkit")
  expect_null(toolkit$info)
})

test_that("hf_detect_engine retries 503 while an endpoint starts", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")

  # the fake returns 503 twice, then the TEI /info response
  detected <- hf_detect_engine(hf_server$url("/tei_cold"), "HF_TEST_API_KEY")
  expect_equal(detected$engine, "tei")
})

test_that("hf_detect_engine errors when the endpoint cannot be reached", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  expect_error(
    hf_detect_engine("http://127.0.0.1:1", "HF_TEST_API_KEY", max_tries = 1),
    "Could not reach"
  )
})

test_that("engine = 'toolkit' skips detection, and an option sets the default", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")

  # an unreachable URL would error if /info were called
  expect_equal(.hf_resolve_engine("toolkit", "http://127.0.0.1:1", "HF_TEST_API_KEY")$engine, "toolkit")
  expect_error(.hf_resolve_engine("vllm", "http://127.0.0.1:1", "HF_TEST_API_KEY"))

  withr::local_options(EndpointR.hf_engine = "toolkit")
  result <- hf_classify_batch(
    texts = c("a", "bb"),
    endpoint_url = hf_server$url("/toolkit_classify"),
    key_name = "HF_TEST_API_KEY",
    concurrent_requests = 1
  ) |> suppressMessages()
  expect_equal(nrow(result), 2)
})

test_that("hf_get_endpoint_info returns NULL for toolkit endpoints", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  expect_equal(hf_get_endpoint_info(hf_server$url("/tei_embed"), "HF_TEST_API_KEY")$version, "1.8.2")
  expect_null(hf_get_endpoint_info(hf_server$url("/toolkit_classify"), "HF_TEST_API_KEY") |> suppressMessages())
})

# request bodies ----

test_that("a batch of 1 is still sent as a JSON array", {
  for (engine in c("tei", "toolkit")) {
    for (task in c("embed", "classify")) {
      body <- hf_batch_body("only text", engine, task)
      json <- jsonlite::fromJSON(httr2::request("http://x") |>
                                   httr2::req_body_json(body) |>
                                   purrr::pluck("body", "data") |>
                                   jsonlite::toJSON(auto_unbox = TRUE),
                                 simplifyVector = FALSE)
      expect_true(is.list(json$inputs), info = paste(engine, task))
      expect_length(json$inputs, 1)
    }
  }
})

test_that("TEI classification nests each text and asks for raw scores", {
  body <- hf_batch_body(c("a", "b"), "tei", "classify")
  expect_equal(body$inputs, list(list("a"), list("b")))
  expect_true(body$truncate)
  expect_true(body$raw_scores)
  expect_null(body$parameters)
})

test_that("TEI embeddings send truncate at the top level, and parameters are merged into it", {
  body <- hf_batch_body(c("a", "b"), "tei", "embed", parameters = list(normalize = FALSE))
  expect_equal(body$inputs, list("a", "b"))
  expect_true(body$truncate)
  expect_false(body$normalize)
  expect_null(body$parameters)
})

test_that("toolkit classification sends batch_size, truncation and max_length", {
  body <- hf_batch_body(c("a", "b", "c"), "toolkit", "classify", max_length = 256L)
  expect_equal(body$inputs, list("a", "b", "c"))
  expect_equal(body$parameters$batch_size, 3)
  expect_true(body$parameters$truncation)
  expect_equal(body$parameters$max_length, 256L)
  expect_true(body$parameters$return_all_scores)

  # user parameters override the defaults
  body <- hf_batch_body(c("a", "b"), "toolkit", "classify", parameters = list(batch_size = 1))
  expect_equal(body$parameters$batch_size, 1)
})

test_that("hf_build_request_batch builds engine-specific requests", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  req <- hf_build_request_batch("a", endpoint_url = "https://x.com", key_name = "HF_TEST_API_KEY",
                                engine = "tei", task = "classify")
  expect_equal(req$body$data$inputs, list(list("a")))
})

# the fake TEI app follows TEI's input rules ----

test_that("the fake TEI classifier reproduces TEI's input rules", {
  url <- hf_server$url("/tei_classify")
  post <- function(body) {
    httr2::request(url) |>
      httr2::req_body_json(body) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform()
  }

  # a flat list of 2 is read as one sentence pair
  pair <- post(list(inputs = list("a", "b")))
  expect_equal(httr2::resp_status(pair), 200)
  expect_length(httr2::resp_body_json(pair), 2) # one result: a list of 2 labels
  expect_named(httr2::resp_body_json(pair)[[1]], c("label", "score"))

  expect_equal(httr2::resp_status(post(list(inputs = list("a", "b", "c")))), 422)
  expect_equal(httr2::resp_status(post(list(inputs = as.list(letters[1:5]) |> purrr::map(list)))), 413)
})

# sending batches ----

test_that("TEI classification returns probabilities in input order", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  texts <- c("short", "a much longer text here", "mid length", "x", "another one of these", "two words")

  result <- hf_classify_batch(
    texts = texts,
    endpoint_url = hf_server$url("/tei_classify"),
    key_name = "HF_TEST_API_KEY",
    batch_size = 2,
    concurrent_requests = 3
  ) |> suppressMessages()

  expect_equal(result$text, texts)
  expect_false(any(result$.error))
  expect_equal(result$spam, stats::plogis(nchar(texts) / 10 - 1), tolerance = 1e-6)
  expect_equal(result$spam + result$not_spam, rep(1, length(texts)), tolerance = 1e-9)
})

test_that("batch_size above max_client_batch_size is lowered with a warning", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  texts <- paste("text", 1:10)

  expect_warning(
    result <- hf_embed_batch(
      texts = texts,
      endpoint_url = hf_server$url("/tei_embed"),
      key_name = "HF_TEST_API_KEY",
      batch_size = 8,
      concurrent_requests = 1
    ) |> suppressMessages(),
    "max_client_batch_size"
  )
  expect_false(any(result$.error))
  expect_equal(result$V1, nchar(texts))
})

test_that("TEI embeddings are sent with top-level truncate", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  result <- hf_embed_batch(
    texts = c("a", "bb", "ccc"),
    endpoint_url = hf_server$url("/tei_embed"),
    key_name = "HF_TEST_API_KEY",
    batch_size = 4,
    concurrent_requests = 1
  ) |> suppressMessages()

  expect_equal(result$V1, c(1, 2, 3))
  expect_equal(result$V2, c(1, 1, 1))
})

test_that("a batch with a NaN text is split and only that text fails", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  texts <- c("fine one", "fine two", "NANTEXT here", "fine three")

  result <- hf_classify_batch(
    texts = texts,
    endpoint_url = hf_server$url("/tei_classify"),
    key_name = "HF_TEST_API_KEY",
    batch_size = 4,
    concurrent_requests = 2
  ) |> suppressMessages()

  expect_equal(result$text, texts)
  expect_equal(result$.error, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(result$.status[3], 424L)
  expect_match(result$.error_msg[3], "score is NaN")
  expect_true(all(is.na(result$spam[3])))
  expect_false(anyNA(result$spam[-3]))
})

test_that("empty and NA texts become error rows and are never sent", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  texts <- c("first", "", NA, "   ", "fifth")

  # the toolkit fake fails the whole batch on an empty text, so any empty text
  # that was sent would make the other texts fail too
  result <- hf_classify_batch(
    texts = texts,
    endpoint_url = hf_server$url("/toolkit_classify"),
    key_name = "HF_TEST_API_KEY",
    batch_size = 32,
    concurrent_requests = 1
  ) |> suppressMessages()

  expect_equal(nrow(result), 5)
  expect_equal(result$.error, c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(unique(result$.error_msg[2:4]), "Empty or missing text, not sent")
  expect_equal(result$positive[c(1, 5)], 1 / (1 + nchar(texts[c(1, 5)])))
})

test_that("toolkit results return in input order after sorting by length and splitting", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  texts <- c("the longest text of them all", "mid text", "BADTEXT", "a", "another fairly long one", "xyz")

  result <- hf_classify_batch(
    texts = texts,
    endpoint_url = hf_server$url("/toolkit_classify"),
    key_name = "HF_TEST_API_KEY",
    batch_size = 3,
    concurrent_requests = 2
  ) |> suppressMessages()

  expect_equal(result$text, texts)
  expect_equal(result$.error, texts == "BADTEXT")
  ok <- texts != "BADTEXT"
  expect_equal(result$positive[ok], 1 / (1 + nchar(texts[ok])))
})

test_that("requests that get 503 are re-sent unchanged", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  # the fake returns 503 for its first POST
  result <- hf_embed_batch(
    texts = c("a", "bb"),
    endpoint_url = hf_server$url("/tei_cold"),
    key_name = "HF_TEST_API_KEY",
    engine = "tei",
    batch_size = 4,
    concurrent_requests = 1
  ) |> suppressMessages()

  expect_false(any(result$.error))
  expect_equal(result$V1, c(1, 2))
})

test_that("a split counts in .hf_send_batches and keeps every row", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  texts <- c("ok", "BADTEXT", "fine", "good")
  sent <- .hf_send_batches(
    list(1:4),
    build_request = \(r) .hf_batch_request(texts[r], hf_server$url("/toolkit_classify"), "fake-key",
                                           "toolkit", "classify"),
    concurrent_requests = 1,
    progress = FALSE
  ) |> suppressMessages()

  expect_equal(sent$n_splits, 2L) # 4 -> 2 + 2, then the half with BADTEXT -> 1 + 1
  expect_setequal(unlist(purrr::map(sent$results, "rows")), 1:4)
})

# chunks and metadata ----

test_that("hf_classify_chunks writes engine details to metadata.json", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  temp_dir <- withr::local_tempdir()
  texts <- c("one", "", "three", "NANTEXT four", "five")

  result <- hf_classify_chunks(
    texts = texts,
    ids = 1:5,
    endpoint_url = hf_server$url("/tei_classify"),
    key_name = "HF_TEST_API_KEY",
    chunk_size = 3,
    batch_size = 4,
    concurrent_requests = 1,
    output_dir = temp_dir
  ) |> suppressMessages()

  expect_equal(result$id, 1:5)
  expect_equal(result$text, texts)
  expect_equal(result$.chunk, c(1, 1, 1, 2, 2))
  expect_equal(result$.error, c(FALSE, TRUE, FALSE, TRUE, FALSE))

  metadata <- jsonlite::read_json(file.path(temp_dir, "metadata.json"))
  expect_equal(metadata$engine, "tei")
  expect_equal(metadata$model_type, "classifier")
  expect_equal(metadata$max_client_batch_size, 4)
  expect_equal(metadata$batch_size, 4)
  expect_equal(metadata$n_empty_texts, 1)
  expect_equal(metadata$n_split_batches, 1)
  expect_true(metadata$truncation_method %in% c("tok", "max_chars"))
  expect_true(metadata$inference_parameters$raw_scores)
})

test_that("hf_embed_df sends batches to TEI and keeps ids in order", {
  withr::local_envvar(HF_TEST_API_KEY = "fake-key")
  df <- data.frame(doc = c("b", "a", "c", "d", "e"), body = c("aa", "b", "cccc", "ddd", "eeeee"))

  result <- hf_embed_df(
    df = df,
    text_var = body,
    id_var = doc,
    endpoint_url = hf_server$url("/tei_embed"),
    key_name = "HF_TEST_API_KEY",
    output_dir = withr::local_tempdir(),
    batch_size = 4,
    concurrent_requests = 2
  ) |> suppressMessages()

  expect_equal(result$doc, df$doc)
  expect_equal(result$V1, nchar(df$body))
})

# tidying TEI responses ----

test_that("tidy_tei_classification_response turns logits into probabilities by label name", {
  # TEI sorts labels by score, so the order differs between texts
  body <- list(
    list(list(label = "spam", score = 2), list(label = "not_spam", score = 0)),
    list(list(label = "not_spam", score = 1), list(label = "spam", score = -1))
  )
  tidied <- tidy_tei_classification_response(body)

  expect_named(tidied, c("spam", "not_spam"), ignore.order = TRUE)
  expect_equal(tidied$spam, stats::plogis(c(2, -2)))
  expect_equal(tidied$spam + tidied$not_spam, c(1, 1))
})

test_that("tidy_tei_classification_response reports NA for a missing or NaN score", {
  body <- list(
    list(list(label = "spam", score = NULL), list(label = "not_spam", score = 0)),
    list(list(label = "spam", score = 0), list(label = "not_spam", score = 0))
  )
  tidied <- tidy_tei_classification_response(body)
  expect_true(is.na(tidied$spam[1]))
  expect_true(is.na(tidied$not_spam[1]))
  expect_equal(tidied$spam[2], 0.5)
})

# cutting texts for TEI classifiers ----

test_that("without a tokeniser, TEI classifier inputs are cut to max_chars", {
  texts <- c(strrep("a", 50), "short", NA)
  cut <- .hf_truncate_for_tei(texts, max_length = 512L, max_chars = 10L, tokenizer = NULL,
                              engine = list(engine = "tei", info = list(model_id = "/repository")),
                              key_name = "HF_TEST_API_KEY") |>
    suppressMessages()

  expect_equal(cut$method, "max_chars")
  expect_equal(cut$texts, c(strrep("a", 10), "short", NA))
  expect_equal(cut$n_cut, 1)

  none <- .hf_truncate_for_tei(texts, max_length = NULL, 10L, NULL, list(), "HF_TEST_API_KEY")
  expect_equal(none$method, "none")
  expect_equal(none$texts, texts)
})

test_that("with tok, TEI classifier inputs are cut to max_length tokens", {
  skip_if_not_installed("tok")
  skip_on_cran()
  skip_if_offline("huggingface.co")

  # no key: the CI dummy key would be rejected, even for a public model
  withr::local_envvar(HF_NO_TEST_API_KEY = "")
  tk <- .hf_load_tokenizer("answerdotai/ModernBERT-base", list(), "HF_NO_TEST_API_KEY")
  skip_if(is.null(tk), "Could not download the ModernBERT tokeniser")

  long <- paste(rep("word", 100), collapse = " ")
  texts <- c(long, "short text", "日本語のテキストはどうなりますか、長い文章です。")
  cut <- .hf_truncate_for_tei(texts, max_length = 10L, max_chars = 2000L, tokenizer = tk,
                              engine = list(), key_name = "HF_TEST_API_KEY")

  expect_equal(cut$method, "tok")
  expect_equal(cut$texts[2], "short text")
  expect_true(startsWith(long, cut$texts[1]))
  expect_lt(nchar(cut$texts[1]), nchar(long))
  # 10 tokens including the 2 special tokens leaves 8 for the text
  expect_lte(length(tk$encode(cut$texts[1], add_special_tokens = FALSE)$ids), 8)
  # truncation is turned off again on the caller's tokeniser
  expect_gt(length(tk$encode(long)$ids), 10)
})
