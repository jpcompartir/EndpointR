# EndpointR Development Guide

## Pre-PR Checklist

-   [ ] export necessary funcs and artefacts
-   [ ] document with examples/full roxygen2 skeleton
-   [ ] check DESCRIPTION for version bump or dependencies update
-   [ ] update news.md
-   [ ] update todos.md
-   [ ] update \_pkgdown.yml (function reference, news etc.)
-   [ ] run `spelling::spell_check_package()`
-   [ ] `devtools::document()`
-   [ ] run `testthat::test()`
-   [ ] run `devtools::check()`
-   [ ] run `pkgdown::build_site()`
-   [ ] inspect site incl. vignettes
-   [ ] PR
-   [ ] Check CI/CD

## Commands

-   Run all tests: `testthat::test()`
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
-   Use lowercase for comments and British English (let AI use uppercase to distinguish - helpful to know whether it's a human or AI comment)

## API Consistency

-   Functions in R/core.R are intended to be 'quite' general - i.e. they form the foundations of the rest of the package
-   Generally we want to deal with single text, batch text, df of texts consistently, or at least predictably differently, across providers and tasks

## AI Usage

-   Don't commit code you don't understand
-   If AI writes the code, you better write the tests. If AI writes the tests, you better write the code.
-   Let AI help you with docs, but make sure you keep them up-to-date. You will only be able to do this if you know what's in the code, and the docs.

## Git Branches

Provide descriptive names where possible

-   feature/\*\*
-   bugfix/\*\*
-   documentation/\*\*
-   tests/\*\*
