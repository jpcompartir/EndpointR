
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

# Installation

``` r
remotes::install_github("jpcompartir/EndpointR")
```

# API Key Security

> **WARNING:** EndpointR will try to help you manage API keys safely,
> but there is a baseline level of responsibility each person using a
> managed service needs to take on, and a set of best practices which
> need to be followed.

The services we are building
<a href="https://en.wikipedia.org/wiki/API" target="_blank">API</a>
connections for will require us to use
<a href="https://aws.amazon.com/what-is/api-key/" target="_blank">API
keys</a> so that API providers can identify us, and give us access to
their services. This presents new security challenges. API keys give us
privileged access to services, in the wrong hands these privileges will
be misused, leading most commonly to large API bills or the leaking of
sensitive data.

It is **paramount** that we handle our API keys securely. You need to
avoid the following things:

- Saving unencrypted API keys in notes, emails, Google Docs etc.
- Sharing API keys with other people
- Including unencrypted API keys in your code (scripts,
  R/Quarto/markdown files, .ipynb etc.)
- Unencypted API keys appearing in your .Rhistory file (**especially**
  if this file is being uploaded anywhere)

If you suspect you may have done one - or any number - of these things,
go directly to where you got your key, invalidate the old one and
generate a new one. If the key was given to you, go directly to the
person and tell them that your key may have been compromised and they
will invalidate it for you, and provide you a new one.

Instead, you need to:

- Encrypt your API keys
- Store them as environment variables if using in R/Rstudio/VScode
- Store them as managed secrets by providers like GitHub for use outside
  of R/Rstudio/VSco de

Read the
<a href="https://httr2.r-lib.org/articles/wrapping-apis.html#basics"
target="_blank">httr2 vignette</a> on managing your API keys securely
and encrypting them. Read the [EndpointR API
Keys](vignettes/api_keys.Rmd) for information on which API keys you need
for wach endpoint we support, and how to securely import those API keys
into your .Renvironfile.
