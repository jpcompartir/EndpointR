get_review_texts <- function() {
  c(
    "Absolutely fantastic service! The staff were incredibly helpful and friendly.",
    "Terrible experience. Food was cold and the waiter was rude.",
    "Pretty good overall, but nothing special. Average food and service.",
    "Outstanding meal! Best restaurant I've been to in years. Highly recommend!",
    "Disappointed with the long wait times. Food was okay when it finally arrived."
  )
}


get_review_df <- function() {
  texts <- get_review_texts()
  review_df <- data.frame(id = 1:length(texts), review_text = texts)
  attr(review_df, "class") <- c("tbl_df", "tbl", "data.frame") # adds the tibble attribute so opens as tbl if user has tibble installed.

  return(review_df)
}

