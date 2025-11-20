# Check the max number of tokens allowed for your inputs

This function requires the model to have 'tokenizer_config.json' file
with a `model_max_length` key, otherwise it will error.

## Usage

``` r
hf_get_model_max_length(model_name, api_key = "HF_API_KEY")
```

## Arguments

- model_name:

  name of the model e.g. 'sentence-transformers/mpnet-base-v2'

- api_key:

  Your Hugging Face auth token

## Value

Integer value of the model_max_length from tokenizer config
