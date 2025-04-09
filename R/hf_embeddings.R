library(httr2)
library(stringr)
library(jsonlite)
library(purrr)

# basic func for getting the list.
tidy_nested_embedding_list <- function(nested_list) {
  tib <- sapply(nested_list, unlist) |>  # first conv to matrix, but this is long format
    t() |> # transpose to wide form
    as.data.frame.matrix() |>  # conv to df, has grim column names
    tibble::as_tibble()

  return(tib)
}

api_key <- Sys.getenv("HF_TEST_API_KEY")

hf_test_api_url <- "https://o1stb590fw4ortu4.us-east-1.aws.endpoints.huggingface.cloud"
sentences <- sentences
one_batch <- toJSON(list(inputs = sentences[1:10]), auto_unbox = FALSE)

# building request out
req <- request(hf_test_api_url)
req <- req |>
  req_user_agent(string = "EndpointR") |>
  req_method("POST") |>
  req_headers("Content-Type" = "application/json") |>
  req_auth_bearer_token(token = api_key) |>
  req_body_json(list(inputs = sentences[1:30])) |>
  req_retry(
    max_tries = 10,
    retry_on_failure = TRUE)

# perform req -> tidy response
resp <- req |>
  httr2::req_perform(verbosity = 1)

resp_json <- resp |>
  resp_body_json()

tidy_nested_embedding_list(resp_json)

# code for tidying that nested list. Would need to figure this out for batching as well, and caching.
sapply(resp_json, unlist) |>  # first conv to matrix, but this is long format
  t() |> # transpose to wide form
  as.data.frame.matrix() |>  # conv to df, has grim column names
  tibble::as_tibble() # get nice column names
