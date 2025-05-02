# space for classifying text data with HF Inference Endpoints
# functions from core and hf_inference will be helpful to re-use
# functions from hf_embed serve as :sparkles: inspo :sparkles:

tidy_classification_response <- function(response){
  if (inherits(response, "httr2_response")) {
    resp_json <- httr2::resp_body_json(response)
  } else {
    resp_json <- response
  }

  # sort later, we're basically going to unlist and pivot?
  # might be better for users to build these themselves and we handle
  # peaks and pits, sentiment, or something
}


