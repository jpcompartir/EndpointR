# Retrieve information about an endpoint

Calls the endpoint's `/info` route. Text Embeddings Inference (TEI)
endpoints return their model, limits and version. Endpoints that run the
default Hugging Face Inference Toolkit have no `/info` route, and this
function returns `NULL` with a message for them.

Retries 429 and 5xx responses, because a scaled-to-zero endpoint returns
503 for about 2 minutes while it starts.

## Usage

``` r
hf_get_endpoint_info(endpoint_url, key_name = "HF_API_KEY", max_tries = 8L)
```

## Arguments

- endpoint_url:

  Hugging Face Inference Endpoint URL

- key_name:

  Name of environment variable containing the API key (default:
  "HF_API_KEY")

- max_tries:

  Maximum attempts for the request (default: 8)

## Value

A list of endpoint information on TEI, or `NULL`
