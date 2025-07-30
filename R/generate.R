
# choose what the API will look like...

generate_text <- function(
    input,
    ...,
    provider = c("openai", "anthropic", "gemini"),
    model = NULL,
    temperature = 0,
    max_tokens = 500L,
    system_prompt = NULL,
    schema = NULL,
    key_name = NULL) {}

generate_batch <- function(
    inputs,
    ...,
    batch_size = 8L,
    concurrent_requests = 5L,
    provider = c("openai", "anthropic", "gemini"),
    model = NULL,
    temperature = 0,
    max_tokens = 500L,
    system_prompt = NULL,
    schema = NULL,
    key_name = NULL
) {}


generate_df <- function(
    df,
    input_var,
    ...,
    batch_size = 8L,
    concurrent_requests = 5L,
    provider = c("openai", "anthropic", "gemini"),
    model = NULL,
    temperature = 0,
    max_tokens = 500L,
    system_prompt = NULL,
    schema = NULL,
    key_name = NULL) {}

oai_complete_text <- function(
    input,
    model = "gpt-4.1-nano",
    temperature = 0,
    max_tokens = 500L,
    schema = NULL,
    system_prompt = NULL,
    key_name = "OPENAI_API_KEY",
    endpoint_url = "https://api.openai.com/v1/chat/completions",
    timeout = 20,
    max_retries = 5) {

}

oai_complete_batch <- function(...) {}

oai_complete_df <- function(...) {}
