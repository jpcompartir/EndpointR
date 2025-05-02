
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EndpointR

<!-- badges: start -->
<!-- badges: end -->

EndpointR is a ‘batteries included’, open-source R package for
connecting to various Application Programming Interfaces
(<a href="https://en.wikipedia.org/wiki/API" target="_blank">APIs</a>)
for Machine Learning model predictions. EndpointR is built for
company-specific use cases at SHARE Creative, so may not be useful to a
wide audience. It is built in the Tidyverse style - functions expect a
data frame as an input and return a data frame as an output.

EndpointR provides access to endpoints via individual functions: one
function - or multiple functions - per endpoint. If you are experienced
with hitting APIs or you need to do something that EndpointR doesn’t
cover, consider going directly to
[httr2](https://httr2.r-lib.org/reference/index.html)

# Installation

EndpointR will not be put on CRAN, so you can download and install it
with the following code:

``` r
remotes::install_github("jpcompartir/EndpointR")
```

# Quick Start

``` r
library(EndpointR)
set_api_key("TEST_API_KEY")

endpoint_url <- httr2::secret_decrypt("kcZCsc92Ty7PuAk7_FdCcdOU_dlDpvWdDyfpwCWg-wW80eJxJPdQ68nz4V_0922SzSwM5_dYfpTOqsZ-GodUpLN4PQbwE73wlZkBCWIaIXc15g", "ENDPOINTR_KEY")

hf_embed_text(
  "Embed this text for me",
  endpoint_url = endpoint_url,
  key_name = "TEST_API_KEY",
  max_retries = 5
)
```

# API Key Security

- Read the
  <a href="https://httr2.r-lib.org/articles/wrapping-apis.html#basics"
  target="_blank">httr2 vignette</a> on managing your API keys securely
  and encrypting them.

- Read the [EndpointR API Keys](vignettes/api_keys.Rmd) vignette for
  information on which API keys you need for wach endpoint we support,
  and how to securely import those API keys into your .Renvironfile.

# Provider-specific functions

We have some provider-specific functions which handle requests
specifically for providers that need them. These functions will:

- belong to a provider
- perform a task (embedding, classification)
- build a request or perform a request
- do so with a strategy (chunked, batch, parallel)

I am still working on how to name these and how to design the API around
them.
