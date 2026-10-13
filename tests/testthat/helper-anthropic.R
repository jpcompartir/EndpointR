# The default model (Haiku 5.5) rejects `temperature`, so the first call that
# keeps the default `temperature = 0` warns once per session. Use that warning
# up here, so it doesn't land on whichever test each worker runs first. Tests
# that check the warning call rlang::reset_warning_verbosity("ant_sampling_drop").
suppressWarnings(
  EndpointR:::.ant_build_body(
    input = "x",
    model = "claude-haiku-5-5",
    max_tokens = 10L,
    temperature = 0,
    system_prompt = NULL,
    schema = NULL,
    effort = NULL,
    thinking = NULL
  )
)
