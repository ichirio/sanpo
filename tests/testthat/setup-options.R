# Engine messages are tested in test-jn-engine.R; silence them elsewhere.
withr::local_options(
  stats::setNames(list(FALSE), opt_name("engine_messages")),
  .local_envir = testthat::teardown_env()
)
