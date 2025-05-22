# EndpointR Development Guide

## Commands

-   Run all tests: `testthat::test_check("EndpointR")`
-   Run single test: `testthat::test_file("tests/testthat/test-tmp.R")`
-   Run specific test: `testthat::test_that("test description", { test code })`
-   Check package: `devtools::check()` or `R CMD check`
-   Build package: `devtools::build()` or `R CMD build .`
-   Generate documentation: `devtools::document()`

## Language Style Guidelines

-   Use British English only
-   Use straightforward language over jargon

## Code Style Guidelines

-   Use roxygen2 with markdown for documentation
-   Follow tidyverse style: snake_case function names, descriptive variables
-   Chain operations with native pipe (`|>`)
-   Use tibble/data.frame for data structures when appropriate
-   Include parameter types in roxygen docs
-   Use tidyeval for data frame functions: capture variable names with `rlang::ensym()` (e.g., `text_sym <- rlang::ensym(text_var)`) for non-standard evaluation
-   Error handling: use `stopifnot()` for assertions and `cli::cli_abort()` for user-facing errors
-   Format error messages with cli syntax (`{.val}`, `{.code}`)
-   Wrap examples requiring API request in `\dontrun{}`
-   Use explicit return values and document them
-   Keep functions small and focused on a single responsibility
-   API keys are saved as environment variables, not input as raw code

## Comment Guidelines

-   Write comments that explain *why* you made the new choice, not stuff like 'Check required parameters' - Let obvious code speak for itself
-   Use lowercase for comments
