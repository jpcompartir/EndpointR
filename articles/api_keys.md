# Managing API Keys Securely

``` r
library(EndpointR)
```

## Quick start

EndpointR has two main functions for managing API keys, they are
[`get_api_key()`](https://jpcompartir.github.io/EndpointR/reference/get_api_key.md)
and
[`set_api_key()`](https://jpcompartir.github.io/EndpointR/reference/set_api_key.md),
they both take ‘key_name’ as an input. For security,
[`set_api_key()`](https://jpcompartir.github.io/EndpointR/reference/set_api_key.md)
uses askpass to accept API keys rather than code - this means it will
not show up in .Rhistory, and is less likely to be leaked.

First you set your API key:

``` r
set_api_key("TOY_API_KEY")
```

Then restart your R session.

Now get your API key

``` r
api_key <- get_api_key("TOY_API_KEY")
```

For security reasons, do not print `api_key` to your console, pass it to
the function that requires it.

## What is an API?

[API](https://en.wikipedia.org/wiki/API) stands for ‘Application
Programming Interface’, it is a mechanism for two pieces of software to
interact and exchange data. APIs work by requests - a user sends a
request to a service, and the service sends a response, fulfilling the
user’s request, or providing an error message.

A specific location within an API that allows the user to access some
service, or function, is called an endpoint.

## What is an API Key?

Because many APIs provide access to sensitive information, or require a
credit card to send requests, it’s important API providers are able to
identify who is sending the request, to determine whether it’s safe to
respond, and who to charge for the response. An API key is *the*
mechanism for identifying who is sending a request.

See also: [API keys](https://aws.amazon.com/what-is/api-key/)

## API Key Security

> **WARNING:** EndpointR will try to help you manage API keys safely,
> but there is a baseline level of responsibility each person using a
> managed service needs to take on, and a set of best practices which
> need to be followed.

It is **paramount** that we handle our API keys securely. You need to
avoid the following things:

- Saving unencrypted API keys in notes, emails, Google Docs etc.
- Uploading API keys to web services (GitHub, etc.)
- Sharing API keys with other people
- Including unencrypted API keys in your code (scripts,
  R/Quarto/markdown files, .ipynb etc.)
- Unencypted API keys appearing in your .Rhistory file (**especially**
  if this file is being uploaded anywhere)

If you suspect you may have done one - or any number - of these things,
go directly to where you got your key, invalidate the old one and
generate a new one. If the key was given to you, go directly to the
person and tell them that your key has been compromised and they will
invalidate it for you, and provide you a new one.

Instead of the above, you should:

- Encrypt your API keys
- Store them as environment variables if using in R/Rstudio/VScode

OR

- Store them as managed secrets by providers like GitHub for use outside
  of R/Rstudio/VScode

## Managing Multiple Keys

For each endpoint that EndpointR provides access to, you will need the
correct environment variable stored in your [.Renviron
file](https://docs.posit.co/ide/user/ide/guide/environments/r/managing-r.html#renviron)

| Endpoint                   | Description                                                               | Environment Variable |
|----------------------------|---------------------------------------------------------------------------|----------------------|
| OpenAI                     | To access OpenAI models for embedding, classification, structured outputs | OPENAI_API_KEY       |
| Anthropic                  | To access Anthropic models for classification and structured outputs      | ANTHROPIC_API_KEY    |
| Hugging Face Inference API | To access models on the Hugging Face Hub via the Inference API            | HF_API_KEY           |
|                            |                                                                           |                      |

API Key Lookup Table
