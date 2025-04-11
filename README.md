
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EndpointR

<!-- badges: start -->
<!-- badges: end -->

EndpointR is a ‘batteries included’, open-source R package for
connecting to various Application Programming Interfaces
(<a href="https://en.wikipedia.org/wiki/API" target="_blank">APIs</a>)
for Machine Learning model predictions. EndpointR is built for
company-specific use cases, so may not be useful to a wide audience. It
is built in the Tidyverse style - functions expect a data frame as an
input and return a data frame as an output.

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

# API Key Security

- Read the
  <a href="https://httr2.r-lib.org/articles/wrapping-apis.html#basics"
  target="_blank">httr2 vignette</a> on managing your API keys securely
  and encrypting them.

- Read the [EndpointR API Keys](vignettes/api_keys.Rmd) vignette for
  information on which API keys you need for wach endpoint we support,
  and how to securely import those API keys into your .Renvironfile.
