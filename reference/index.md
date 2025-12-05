# Package index

## Getting Started

Essential functions for setting up and managing API keys

- [`set_api_key()`](https://jpcompartir.github.io/EndpointR/reference/set_api_key.md)
  : Set your API keys so they can be accessed by EndpointR
- [`get_api_key()`](https://jpcompartir.github.io/EndpointR/reference/get_api_key.md)
  : Retrieve an API key which has been stored as an Environment
  Variable.

## Hugging Face - Text Embeddings

Functions for generating text embeddings using Hugging Face endpoints

- [`hf_embed_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_text.md)
  : Generate embeddings for a single text
- [`hf_embed_batch()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_batch.md)
  : Generate batches of embeddings for a list of texts
- [`hf_embed_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_chunks.md)
  : Embed text chunks through Hugging Face Inference Embedding Endpoints
- [`hf_embed_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_embed_df.md)
  : Generate embeddings for texts in a data frame
- [`tidy_embedding_response()`](https://jpcompartir.github.io/EndpointR/reference/tidy_embedding_response.md)
  : Process embedding API response into a tidy format

## Hugging Face - Text Classification

Functions for classifying text using Hugging Face endpoints

- [`hf_classify_text()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_text.md)
  : Classify text using a Hugging Face Inference API endpoint
- [`hf_classify_batch()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_batch.md)
  : Classify multiple texts using Hugging Face Inference Endpoints
- [`hf_classify_chunks()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_chunks.md)
  : Efficiently classify vectors of text in chunks
- [`hf_classify_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_classify_df.md)
  : Classify a data frame of texts using Hugging Face Inference
  Endpoints
- [`tidy_classification_response()`](https://jpcompartir.github.io/EndpointR/reference/tidy_classification_response.md)
  : Convert Hugging Face classification response to tidy format

## Hugging Face - Core Infrastructure

Low-level functions for building and performing Hugging Face requests

- [`hf_build_request()`](https://jpcompartir.github.io/EndpointR/reference/hf_build_request.md)
  : Prepare a single text embedding request
- [`hf_build_request_batch()`](https://jpcompartir.github.io/EndpointR/reference/hf_build_request_batch.md)
  : Prepare a batch request for multiple texts
- [`hf_build_request_df()`](https://jpcompartir.github.io/EndpointR/reference/hf_build_request_df.md)
  : Prepare embedding requests for texts in a data frame
- [`hf_perform_request()`](https://jpcompartir.github.io/EndpointR/reference/hf_perform_request.md)
  : Execute a single embedding request and process the response

## Hugging Face Endpoint Utilities

Small functions for checking things with Hugging FACE APIs

- [`hf_get_endpoint_info()`](https://jpcompartir.github.io/EndpointR/reference/hf_get_endpoint_info.md)
  : Retrieve information about an endpoint
- [`hf_get_model_max_length()`](https://jpcompartir.github.io/EndpointR/reference/hf_get_model_max_length.md)
  : Check the max number of tokens allowed for your inputs

## Anthropic Messages

functions for working with Anthropic’s Messages API

- [`ant_build_messages_request()`](https://jpcompartir.github.io/EndpointR/reference/ant_build_messages_request.md)
  : Build an Anthropic Messages API request
- [`ant_complete_text()`](https://jpcompartir.github.io/EndpointR/reference/ant_complete_text.md)
  : Generate a completion for a single text using Anthropic's Messages
  API
- [`ant_complete_chunks()`](https://jpcompartir.github.io/EndpointR/reference/ant_complete_chunks.md)
  : Process text chunks through Anthropic's Messages API with batch file
  output
- [`ant_complete_df()`](https://jpcompartir.github.io/EndpointR/reference/ant_complete_df.md)
  : Process a data frame through Anthropic's Messages API

## OpenAI Completions

Functions for working with OpenAI’s APIs including structured outputs

- [`oai_build_completions_request()`](https://jpcompartir.github.io/EndpointR/reference/oai_build_completions_request.md)
  : Build an OpenAI API Chat Completions request
- [`oai_build_completions_request_list()`](https://jpcompartir.github.io/EndpointR/reference/oai_build_completions_request_list.md)
  : Build OpenAI requests for batch processing
- [`oai_complete_text()`](https://jpcompartir.github.io/EndpointR/reference/oai_complete_text.md)
  : Generate a completion for a single text using OpenAI's Chat
  Completions API
- [`oai_complete_chunks()`](https://jpcompartir.github.io/EndpointR/reference/oai_complete_chunks.md)
  : Process text chunks through OpenAI's Chat Completions API with batch
  file output
- [`oai_complete_df()`](https://jpcompartir.github.io/EndpointR/reference/oai_complete_df.md)
  : Process a data frame through OpenAI's Chat Completions API with
  chunked processing

## OpenAI Embeddings

Functions for extracting embeddings with OpenAI’s text embedding models

- [`oai_build_embedding_request()`](https://jpcompartir.github.io/EndpointR/reference/oai_build_embedding_request.md)
  : Build OpenAI embedding API request
- [`oai_embed_text()`](https://jpcompartir.github.io/EndpointR/reference/oai_embed_text.md)
  : Generate embeddings for a single text using OpenAI
- [`oai_embed_batch()`](https://jpcompartir.github.io/EndpointR/reference/oai_embed_batch.md)
  : Generate embeddings for multiple texts using OpenAI
- [`oai_embed_chunks()`](https://jpcompartir.github.io/EndpointR/reference/oai_embed_chunks.md)
  : Embed text chunks through OpenAI's Embeddings API
- [`oai_embed_df()`](https://jpcompartir.github.io/EndpointR/reference/oai_embed_df.md)
  : Generate embeddings for texts in a data frame using OpenAI
- [`tidy_oai_embedding()`](https://jpcompartir.github.io/EndpointR/reference/tidy_oai_embedding.md)
  : Process OpenAI embedding API response into a tidy format

## JSON Schema for Structured Outputs

Type-safe schema creation and validation for structured LLM outputs

- [`create_json_schema()`](https://jpcompartir.github.io/EndpointR/reference/create_json_schema.md)
  : Create a JSON Schema object
- [`json_schema()`](https://jpcompartir.github.io/EndpointR/reference/json_schema.md)
  : Create JSON Schema S7 class for structured outputs
- [`json_dump`](https://jpcompartir.github.io/EndpointR/reference/json_dump.md)
  : Convert json_schema to API format
- [`validate_response`](https://jpcompartir.github.io/EndpointR/reference/validate_response.md)
  : Validate response data against schema

## Schema Builders

Helper functions for creating different types of JSON schema properties

- [`schema_object()`](https://jpcompartir.github.io/EndpointR/reference/schema_object.md)
  : Create JSON Schema object definitions
- [`schema_string()`](https://jpcompartir.github.io/EndpointR/reference/schema_string.md)
  : Create string property schema
- [`schema_number()`](https://jpcompartir.github.io/EndpointR/reference/schema_number.md)
  : Create numeric property schema
- [`schema_integer()`](https://jpcompartir.github.io/EndpointR/reference/schema_integer.md)
  : Create integer property schema
- [`schema_boolean()`](https://jpcompartir.github.io/EndpointR/reference/schema_boolean.md)
  : Create boolean property schema
- [`schema_enum()`](https://jpcompartir.github.io/EndpointR/reference/schema_enum.md)
  : Create enumerated property schema
- [`schema_array()`](https://jpcompartir.github.io/EndpointR/reference/schema_array.md)
  : Create array property schema

## Core Utilities

Internal utility functions and data processing helpers

- [`safely_perform_request()`](https://jpcompartir.github.io/EndpointR/reference/safely_perform_request.md)
  : Safely perform an embedding request with error handling
- [`chunk_dataframe()`](https://jpcompartir.github.io/EndpointR/reference/chunk_dataframe.md)
  : Split a data frame into chunks for batch processing
- [`perform_requests_with_strategy()`](https://jpcompartir.github.io/EndpointR/reference/perform_requests_with_strategy.md)
  : Perform multiple requests with configurable concurrency strategy
- [`process_response()`](https://jpcompartir.github.io/EndpointR/reference/process_response.md)
  : Process API response with error handling
- [`hf_perform_request()`](https://jpcompartir.github.io/EndpointR/reference/hf_perform_request.md)
  : Execute a single embedding request and process the response
- [`validate_hf_endpoint()`](https://jpcompartir.github.io/EndpointR/reference/validate_hf_endpoint.md)
  : Validate that a Hugging Face Inference Endpoint is available
- [`base_request()`](https://jpcompartir.github.io/EndpointR/reference/base_request.md)
  : Create a base HTTP POST request for API endpoints
- [`safely_from_json()`](https://jpcompartir.github.io/EndpointR/reference/safely_from_json.md)
  : Safely extract JSON

## Sample Data

Datasets included with the package for examples and testing

- [`batch_concurrent_benchmark`](https://jpcompartir.github.io/EndpointR/reference/batch_concurrent_benchmark.md)
  : Batch concurrent benchmark results
- [`sentiment_classification_example`](https://jpcompartir.github.io/EndpointR/reference/sentiment_classification_example.md)
  : Single sentiment classification result example
- [`df_sentiment_classification_example`](https://jpcompartir.github.io/EndpointR/reference/df_sentiment_classification_example.md)
  : Example sentiment classification results from Hugging Face API
- [`single_embedding_hf`](https://jpcompartir.github.io/EndpointR/reference/single_embedding_hf.md)
  : Single embedding result example from Hugging Face API
- [`df_embeddings_hf`](https://jpcompartir.github.io/EndpointR/reference/df_embeddings_hf.md)
  : Example embedding results from Hugging Face API
