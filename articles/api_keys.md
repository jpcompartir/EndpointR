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
- Unencrypted API keys appearing in your .Rhistory file (**especially**
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

## Encrypting Your API Keys

To avoid exposing your API keys in plain text it’s sensible to encrypt
them. The basic flow is to:

1.  Create an encryption key
2.  Store this key securely, somewhere separate to where your other API
    keys are stored
3.  Use this encryption key to encrypt your other API keys. Use R’s
    input tool or a package like {askpass} to avoid inputting the key in
    your R Session
4.  Decrypt your API keys whenever you need them, using your encryption
    key

Here’s an example in code where we make use of httr2’s
`secret_make_key`, `secret_encrypt` and `secret_decrypt` functions to
achieve this.

First we make an encryption key, remembering not to print it to the
console/view its contents in our session

``` r
SUPER_SECRET_ENCRYPTION_DEVICE <- httr2::secret_make_key() # store this separately to your API keys, but still somewhere you can retrieve/access it from.
```

Then we take some information that we want to encrypt, and input it with
askpass:

``` r
info <- askpass::askpass()
encrypted_info <- httr2::secret_encrypt(info, SUPER_SECRET_ENCRYPTION_DEVICE)
encrypted_info
```

    #> [1] "GaYCB3u0FJCwXnFdmej-XodpBvgH-7GOprJP_fyGvnQqxxmEdKdQeJBI7WVgWxvs2RgT"

Now our secret is encrypted, we decrypt it to get the original
information back.

``` r
httr2::secret_decrypt(encrypted_info, SUPER_SECRET_ENCRYPTION_DEVICE)
#> [1] "What's the worst that could happen?"
```
