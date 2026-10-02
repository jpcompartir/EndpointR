# Internal helpers for Hugging Face Inference Endpoints. Our endpoints run one
# of two inference engines, which need different request bodies:
#   - Text Embeddings Inference (TEI), which has a GET /info route
#   - the default Hugging Face Inference Toolkit, which does not
# These helpers detect the engine, build request bodies for it, send batches of
# texts and split failed batches, and cut long texts for TEI classifiers.

.hf_transient_status <- c(429L, 502L, 503L, 504L)
.hf_split_status <- c(400L, 413L, 422L, 424L)

.hf_engine_cache <- new.env(parent = emptyenv())

# engine detection ----

#' Fetch the response from an endpoint's /info route
#'
#' @description
#' TEI endpoints answer `GET {endpoint_url}/info`. Toolkit endpoints do not.
#' Retries 429 and 5xx responses, because a scaled-to-zero endpoint returns 503
#' for about 2 minutes while it starts.
#'
#' @return The httr2 response, or the error condition for a network failure.
#' @noRd
.hf_fetch_info <- function(endpoint_url, key_name = "HF_API_KEY", max_tries = 8L) {
  req <- httr2::request(paste0(sub("/+$", "", endpoint_url), "/info")) |>
    httr2::req_user_agent("EndpointR") |>
    httr2::req_auth_bearer_token(get_api_key(key_name)) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(
      max_tries = max_tries,
      is_transient = \(resp) httr2::resp_status(resp) %in% .hf_transient_status,
      backoff = \(i) min(2^i, 30)
    ) |>
    httr2::req_error(is_error = \(resp) FALSE)

  tryCatch(httr2::req_perform(req), error = \(e) e)
}

#' Detect whether an endpoint runs TEI or the Inference Toolkit
#'
#' @description
#' Calls `GET {endpoint_url}/info`. A 200 response with `max_client_batch_size`
#' means TEI. A 4xx response (other than 401 and 403) means the toolkit.
#' Network errors, auth errors and 5xx responses after retries stop with an
#' error, because the endpoint cannot be used.
#'
#' @param endpoint_url The URL of the Hugging Face Inference endpoint
#' @param key_name Name of the environment variable containing the API key
#' @param max_tries Maximum attempts for the /info request
#'
#' @return A list with `engine` (`"tei"` or `"toolkit"`) and `info` (the parsed
#'   /info JSON on TEI, `NULL` on the toolkit)
#' @noRd
hf_detect_engine <- function(endpoint_url, key_name = "HF_API_KEY", max_tries = 8L) {
  resp <- .hf_fetch_info(endpoint_url, key_name, max_tries)

  if (!inherits(resp, "httr2_response")) {
    cli::cli_abort(c(
      "Could not reach {.url {endpoint_url}} to detect its inference engine.",
      "x" = conditionMessage(resp),
      "i" = "Check the URL, or set {.code engine = \"tei\"} or {.code engine = \"toolkit\"} to skip detection."
    ))
  }

  status <- httr2::resp_status(resp)

  if (status == 200) {
    info <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
    if (!is.null(info$max_client_batch_size)) {
      return(list(engine = "tei", info = info))
    }
    return(list(engine = "toolkit", info = NULL))
  }

  if (status %in% c(401L, 403L) || status >= 500) {
    cli::cli_abort(c(
      "{.url {endpoint_url}} returned HTTP {status} when detecting its inference engine.",
      "x" = .extract_api_error(resp),
      "i" = "If the endpoint is starting up, wait and try again.",
      "i" = "Set {.code engine = \"tei\"} or {.code engine = \"toolkit\"} to skip detection."
    ))
  }

  list(engine = "toolkit", info = NULL)
}

#' Resolve the `engine` argument of the hf_* functions
#'
#' @description
#' `"auto"` detects the engine once per endpoint URL per session and caches the
#' result. `"tei"` and `"toolkit"` skip detection; with `"tei"` the function
#' still tries /info once (without retries) to learn the endpoint's limits.
#'
#' @return A list with `engine` and `info`
#' @noRd
.hf_resolve_engine <- function(engine, endpoint_url, key_name) {
  engine <- rlang::arg_match(engine, c("auto", "tei", "toolkit"))

  if (engine == "toolkit") {
    return(list(engine = "toolkit", info = NULL))
  }

  cached <- .hf_engine_cache[[endpoint_url]]
  if (!is.null(cached) && (engine == "auto" || cached$engine == engine)) {
    return(cached)
  }

  if (engine == "tei") {
    resp <- .hf_fetch_info(endpoint_url, key_name, max_tries = 1L)
    info <- NULL
    if (inherits(resp, "httr2_response") && httr2::resp_status(resp) == 200) {
      info <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
    }
    return(list(engine = "tei", info = info))
  }

  detected <- hf_detect_engine(endpoint_url, key_name)
  assign(endpoint_url, detected, envir = .hf_engine_cache)
  cli::cli_alert_info("{.url {endpoint_url}} runs {.val {detected$engine}}")
  detected
}

#' Lower batch_size to the endpoint's max_client_batch_size
#'
#' @noRd
.hf_check_batch_size <- function(batch_size, engine) {
  max_batch <- engine$info$max_client_batch_size
  if (engine$engine == "tei" && !is.null(max_batch) && batch_size > max_batch) {
    cli::cli_warn(c(
      "{.arg batch_size} ({batch_size}) is above the endpoint's {.field max_client_batch_size} ({max_batch}).",
      "i" = "Using {.code batch_size = {max_batch}}."
    ))
    return(as.integer(max_batch))
  }
  as.integer(batch_size)
}

#' Engine fields to record in metadata.json
#'
#' @noRd
.hf_engine_metadata <- function(engine) {
  fields <- c("version", "model_id", "model_type", "max_input_length",
              "max_batch_tokens", "max_client_batch_size", "auto_truncate")
  info <- engine$info[intersect(fields, names(engine$info))]
  # model_type is a nested object on TEI, e.g. {"classifier": {"id2label": ...}}
  if (is.list(info$model_type)) {
    info$model_type <- names(info$model_type)[[1]]
  }
  c(list(engine = engine$engine), info)
}

# request bodies ----

#' Build a request body for a batch of texts
#'
#' @description
#' Bodies differ by engine and task:
#'
#' * TEI classification sends `[[text], [text]]`, because TEI reads a flat list
#'   of 2 texts as one sentence pair. It also sets `raw_scores = TRUE`, because
#'   a NaN score crashes TEI's softmax; EndpointR applies the softmax instead.
#' * TEI embeddings send `truncate` at the top level. TEI ignores `parameters`,
#'   so any `parameters` are merged into the top level of the body.
#' * Toolkit classification sets `parameters$batch_size`, without which the
#'   pipeline runs one text at a time on the GPU.
#'
#' Inputs are always sent as a JSON array, even for a batch of 1.
#'
#' @param texts Character vector of texts
#' @param engine `"tei"` or `"toolkit"`
#' @param task `"embed"` or `"classify"`
#' @param max_length Maximum tokens per text, used by toolkit classification
#' @param parameters Extra parameters, which override the defaults
#'
#' @return A list to send with `httr2::req_body_json()`
#' @noRd
hf_batch_body <- function(texts, engine, task, max_length = 512L, parameters = list()) {
  texts <- as.character(texts)

  if (engine == "tei") {
    if (task == "classify") {
      body <- list(inputs = lapply(texts, list), truncate = TRUE, raw_scores = TRUE)
    } else {
      body <- list(inputs = as.list(texts), truncate = TRUE)
    }
    return(utils::modifyList(body, parameters))
  }

  if (task == "classify") {
    defaults <- list(
      return_all_scores = TRUE,
      truncation = TRUE,
      max_length = max_length,
      batch_size = length(texts)
    )
    return(list(inputs = as.list(texts), parameters = utils::modifyList(defaults, parameters)))
  }

  # toolkit embeddings: none of our embedding endpoints use the toolkit, so this
  # path is untested against a live endpoint
  body <- list(inputs = as.list(texts))
  if (length(parameters) > 0) {
    body$parameters <- parameters
  }
  body
}

#' Build a request for a batch of texts, without a retry policy
#'
#' @description
#' Used by the send loop, which retries and splits batches itself. httr2 does
#' not retry parallel requests whose errors are returned as responses, so
#' leaving retries to the loop keeps sequential and parallel runs the same.
#'
#' @noRd
.hf_batch_request <- function(texts, endpoint_url, api_key, engine, task,
                              max_length = 512L, parameters = list(), timeout = 120) {
  base_request(endpoint_url, api_key) |>
    httr2::req_body_json(hf_batch_body(texts, engine, task, max_length, parameters)) |>
    httr2::req_timeout(timeout)
}

# sending batches ----

#' Send batches of texts, retrying and splitting failed batches
#'
#' @description
#' Sends each batch, then:
#'
#' * keeps a batch whose response has one result per text
#' * re-sends a batch that got 429, 502, 503 or 504, up to `max_retries` times
#' * splits a batch in half when it gets 400, 413, 422 or 424, a network
#'   error, or the wrong number of results, so only the text at fault fails
#' * re-sends a single text after a network error, up to `max_retries` times
#'
#' @param batches List of integer vectors, the positions of the texts in each batch
#' @param build_request Function that takes positions and returns an httr2 request
#' @param concurrent_requests Number of requests in flight
#' @param max_retries Maximum re-sends of a batch after transient errors
#' @param progress Whether to show a progress bar
#'
#' @return A list with `results` (one entry per finished batch, with `rows`,
#'   `response`, `error` and `status`) and `n_splits`
#' @noRd
.hf_send_batches <- function(batches, build_request, concurrent_requests,
                             max_retries = 5L, progress = TRUE) {
  pending <- purrr::map(unname(batches), \(rows) list(rows = rows, tries = 0L))
  results <- list()
  n_splits <- 0L

  finish <- function(rows, response = NULL, error = NA_character_, status = NA_integer_) {
    results[[length(results) + 1]] <<- list(rows = rows, response = response, error = error, status = status)
  }

  while (length(pending) > 0) {
    requests <- purrr::map(pending, \(b) build_request(b$rows))
    responses <- perform_requests_with_strategy(
      requests,
      concurrent_requests = concurrent_requests,
      progress = progress
    )

    next_round <- list()
    wait <- 0

    for (k in seq_along(pending)) {
      rows <- pending[[k]]$rows
      tries <- pending[[k]]$tries
      resp <- responses[[k]]
      is_resp <- inherits(resp, "httr2_response")
      status <- if (is_resp) httr2::resp_status(resp) else NA_integer_
      can_split <- length(rows) > 1

      split_batch <- function() {
        half <- ceiling(length(rows) / 2)
        next_round <<- c(next_round, list(
          list(rows = rows[seq_len(half)], tries = tries),
          list(rows = rows[-seq_len(half)], tries = tries)
        ))
        n_splits <<- n_splits + 1L
      }

      if (is_resp && status < 400) {
        body <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
        if (!is.null(body) && length(body) == length(rows)) {
          finish(rows, response = resp)
        } else if (can_split) {
          split_batch()
        } else {
          finish(rows, error = glue::glue("Endpoint returned {length(body)} results for {length(rows)} text(s)"),
                 status = status)
        }
      } else if (is_resp && status %in% .hf_transient_status && tries < max_retries) {
        next_round <- c(next_round, list(list(rows = rows, tries = tries + 1L)))
        wait <- max(wait, min(2^(tries + 1), 30))
      } else if (can_split && (!is_resp || status %in% .hf_split_status)) {
        split_batch()
      } else if (!is_resp && tries < max_retries) {
        next_round <- c(next_round, list(list(rows = rows, tries = tries + 1L)))
        wait <- max(wait, min(2^(tries + 1), 30))
      } else {
        finish(rows, error = .extract_api_error(resp, "Request failed"), status = status)
      }
    }

    if (length(next_round) > 0 && wait > 0) {
      Sys.sleep(wait)
    }
    pending <- next_round
  }

  list(results = results, n_splits = n_splits)
}

#' Send texts to an endpoint in batches and return one row per text
#'
#' @description
#' The shared engine behind the `hf_*_batch()` and `hf_*_chunks()` functions.
#' Drops empty and missing texts before sending and reports them as errors,
#' sorts texts by length on the toolkit (which pads each batch to its longest
#' text), splits the texts into batches, sends them, and returns the results in
#' input order.
#'
#' @param texts Character vector of texts
#' @param tidy_func Function that turns a response into a tibble with one row per text
#'
#' @return A list with `results`, a tibble with `.row` (position in `texts`),
#'   `.error`, `.error_msg`, `.status` and the result columns, in input order;
#'   `n_empty` and `n_splits`
#' @noRd
.hf_process_texts <- function(texts, endpoint_url, api_key, engine, task,
                              tidy_func, batch_size, concurrent_requests,
                              max_retries, timeout, max_length = 512L,
                              parameters = list(), progress = TRUE) {
  texts <- as.character(texts)
  is_empty <- is.na(texts) | !nzchar(trimws(texts))
  rows <- which(!is_empty)

  results <- list()
  n_splits <- 0L

  if (length(rows) > 0) {
    if (engine == "toolkit") {
      rows <- rows[order(nchar(texts[rows], type = "chars"))]
    }
    batches <- split(rows, ceiling(seq_along(rows) / batch_size))

    sent <- .hf_send_batches(
      batches,
      build_request = \(r) .hf_batch_request(texts[r], endpoint_url, api_key, engine, task,
                                             max_length, parameters, timeout),
      concurrent_requests = concurrent_requests,
      max_retries = max_retries,
      progress = progress
    )
    n_splits <- sent$n_splits

    results <- purrr::map(sent$results, \(batch) {
      if (is.null(batch$response)) {
        return(.hf_error_rows(batch$rows, batch$error, batch$status))
      }
      tryCatch({
        tidied <- tibble::as_tibble(tidy_func(batch$response))
        if (nrow(tidied) != length(batch$rows)) {
          cli::cli_abort("tidy_func returned {nrow(tidied)} rows for {length(batch$rows)} texts")
        }
        tibble::tibble(.row = batch$rows, .error = FALSE, .error_msg = NA_character_,
                       .status = NA_integer_) |>
          dplyr::bind_cols(tidied)
      }, error = \(e) {
        cli::cli_warn("Error processing response: {conditionMessage(e)}")
        .hf_error_rows(batch$rows, conditionMessage(e))
      })
    })
  }

  if (any(is_empty)) {
    results <- c(results, list(.hf_error_rows(which(is_empty), "Empty or missing text, not sent")))
  }

  out <- dplyr::bind_rows(results)
  out <- out[order(out$.row), ]

  list(results = out, n_empty = sum(is_empty), n_splits = n_splits)
}

.hf_error_rows <- function(rows, error_msg, status = NA_integer_) {
  tibble::tibble(.row = rows, .error = TRUE, .error_msg = as.character(error_msg),
                 .status = as.integer(status))
}

#' Pick the default tidy function for a task and engine
#'
#' @noRd
.hf_default_tidy <- function(task, engine) {
  if (task == "embed") return(tidy_embedding_response)
  if (engine == "tei") return(tidy_tei_classification_response)
  tidy_batch_classification_response
}

# cutting texts for TEI classifiers ----

#' Cut classifier inputs to max_length tokens on the client, for TEI
#'
#' @description
#' TEI cuts texts only at the model's limit (8,192 tokens for ModernBERT) and
#' ignores a per-request `max_length`. Long texts can give NaN scores in fp16,
#' so EndpointR cuts them before sending:
#'
#' * with the `tok` package and the model's tokeniser, each text longer than
#'   `max_length` tokens is replaced by the decoded text of its first
#'   `max_length` tokens (special tokens included, as on the toolkit)
#' * otherwise each text is cut to `max_chars` characters
#'
#' @param texts Character vector
#' @param max_length Maximum tokens per text. `NULL` turns cutting off.
#' @param max_chars Character limit used when no tokeniser is available
#' @param tokenizer `NULL`, a Hugging Face model id, or a `tok::tokenizer`
#' @param engine The resolved engine list, used for the model id in /info
#' @param key_name API key used to download a private tokeniser
#'
#' @return A list with `texts`, `method` (`"tok"`, `"max_chars"` or `"none"`)
#'   and `n_cut`
#' @noRd
.hf_truncate_for_tei <- function(texts, max_length, max_chars, tokenizer, engine, key_name) {
  if (is.null(max_length)) {
    return(list(texts = texts, method = "none", n_cut = 0L))
  }

  tk <- .hf_load_tokenizer(tokenizer, engine, key_name)

  if (!is.null(tk)) {
    cut <- .hf_tok_truncate(texts, tk, max_length)
    return(list(texts = cut$texts, method = "tok", n_cut = cut$n_cut))
  }

  cli::cli_inform(c(
    "i" = "TEI classifiers ignore {.arg max_length}, so EndpointR cuts texts to {max_chars} characters before sending.",
    " " = "Some scripts use more than one token per character, so this is not a guaranteed {max_length} token limit.",
    " " = "Install {.pkg tok} and pass {.arg tokenizer} (a model id) to cut at exactly {max_length} tokens."
  ))
  too_long <- !is.na(texts) & nchar(texts, type = "chars", allowNA = TRUE) > max_chars
  texts[too_long] <- substr(texts[too_long], 1, max_chars)
  list(texts = texts, method = "max_chars", n_cut = sum(too_long))
}

#' Load a tokeniser with tok, or return NULL
#'
#' @noRd
.hf_load_tokenizer <- function(tokenizer, engine, key_name) {
  if (inherits(tokenizer, "tok_tokenizer")) {
    return(tokenizer)
  }
  if (!requireNamespace("tok", quietly = TRUE)) {
    return(NULL)
  }

  model_id <- if (is.null(tokenizer)) engine$info$model_id else tokenizer
  # dedicated endpoints report their model as "/repository"
  if (is.null(model_id) || startsWith(model_id, "/")) {
    return(NULL)
  }

  tryCatch({
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    req <- httr2::request(glue::glue("https://huggingface.co/{model_id}/resolve/main/tokenizer.json")) |>
      httr2::req_user_agent("EndpointR") |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries = 3)
    api_key <- Sys.getenv(key_name)
    if (nzchar(api_key)) {
      req <- httr2::req_auth_bearer_token(req, api_key)
    }
    httr2::req_perform(req, path = path)
    tok::tokenizer$from_file(path)
  }, error = \(e) {
    cli::cli_warn(c(
      "Could not load the tokeniser for {.val {model_id}}, falling back to a character limit.",
      "x" = conditionMessage(e)
    ))
    NULL
  })
}

#' Cut texts to their first max_length tokens with a tok tokeniser
#'
#' @noRd
.hf_tok_truncate <- function(texts, tk, max_length) {
  # a token covers at least one byte, so texts this short cannot be too long
  candidates <- which(!is.na(texts) & nchar(texts, type = "bytes", allowNA = TRUE) > max_length - 2)
  if (length(candidates) == 0) {
    return(list(texts = texts, n_cut = 0L))
  }

  # tok tokenisers wrap a pointer, so turn truncation off again for the caller
  tk$enable_truncation(max_length)
  on.exit(tk$no_truncation(), add = TRUE)
  encodings <- tk$encode_batch(texts[candidates])
  # texts that hit max_length may have been cut; decoding the kept ids gives
  # back the covered text, exactly for byte-level BPE tokenisers
  cut <- purrr::map_int(encodings, \(e) length(e$ids)) >= max_length
  if (any(cut)) {
    texts[candidates[cut]] <- tk$decode_batch(purrr::map(encodings[cut], \(e) e$ids))
  }
  list(texts = texts, n_cut = sum(cut))
}

# tidying TEI classifier responses ----

#' Tidy a TEI classification response with raw scores
#'
#' @description
#' With `raw_scores = TRUE`, TEI returns logits. This function applies the
#' softmax in R. TEI sorts each text's labels by score, so labels are matched
#' by name, never by position. A missing or NaN score gives `NA` for that text.
#'
#' @param response An httr2 response, or the parsed JSON: a list with one
#'   element per text, each a list of `{label, score}` objects
#'
#' @return A tibble with one row per text and one column per label
#' @export
tidy_tei_classification_response <- function(response) {
  resp_json <- if (inherits(response, "httr2_response")) httr2::resp_body_json(response) else response

  purrr::map(resp_json, \(item) {
    labels <- purrr::map_chr(item, "label")
    logits <- purrr::map_dbl(item, \(x) if (is.null(x$score)) NA_real_ else as.numeric(x$score))
    probs <- exp(logits - max(logits))
    probs <- probs / sum(probs)
    tibble::as_tibble_row(as.list(stats::setNames(probs, labels)))
  }) |>
    purrr::list_rbind()
}

# chunk files ----

.hf_write_metadata <- function(metadata, output_dir) {
  jsonlite::write_json(metadata,
                       file.path(output_dir, "metadata.json"),
                       auto_unbox = TRUE,
                       pretty = TRUE)
}

.hf_write_chunk <- function(chunk_df, output_dir, chunk_num) {
  if (nrow(chunk_df) > 0) {
    arrow::write_parquet(chunk_df, file.path(output_dir, sprintf("chunk_%03d.parquet", chunk_num)))
  }
}

.hf_read_chunks <- function(output_dir) {
  parquet_files <- sort(list.files(output_dir, pattern = "\\.parquet$", full.names = TRUE))
  # read file by file to keep chunk order; open_dataset() does not guarantee it
  purrr::map(parquet_files, arrow::read_parquet) |>
    dplyr::bind_rows()
}
