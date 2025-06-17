oai_build_embedding_request <- function(input,
                                        model = "text-embedding-3-small",
                                        timeout = 20,
                                        max_retries = 5,
                                        endpoint_url = "https://api.openai.com/v1/embeddings",
                                        key_name = "OPENAI_API_KEY") {

}


oai_embed_text <- function(text, model = "text-embedding-3-small",endpoint_url = "https://api.openai.com/v1/embeddings", key_name = "OPENAI_API_KEY") {

}

oai_embed_list <- function(texts, model = "text-embedding-3-small", concurrent_requests = 1, max_retries = 5, timeout = 20, endpoint_url = "https://api.openai.com/v1/embeddings", key_name = "OPENAI_API_KEY") {

}

oai_embed_df <- function(df, text_var, id_var, max_retries = 5, timeout = 20, model = "text-embedding-3-small",endpoint_url = "https://api.openai.com/v1/embeddings", key_name = "OPENAI_API_KEY" ) {

}
