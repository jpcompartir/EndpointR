library(httr2)
library(stringr)
library(jsonlite)
library(purrr)

embed_hf <- function(endpoint_url, api_key = api_key)

batch_embed_hf <- function(endpoint_url, api_key = api_key, batch_size = 10, max_retries = 5) {

}

# basic func for getting the list tidied.
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
  req_progress() |>
  req_body_json(list(inputs = sentences[[1]])) |>
  # req_body_json(list(inputs = sentences[31:60])) |>
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


# df approach - need to figure out the best way to handle batches, and whether to cache or not - I think caching is probably a good idea.
id <-paste0("uid_", 1:length(sentences))
request_df <- tibble(
  id = id,
  text = sentences
) |>
  mutate(request = map(text, ~ build_hf_embed_request(.x, hf_test_api_url, api_key = api_key)
  ))

response_df <-request_df |>
  mutate(response = map(request, ~ req_perform(.x)))

resp <- req |>
  req_perform(path = )


response_df |>  pluck('response', 1) |>  resp_body_json() |>  tidy_nested_embedding_list()
