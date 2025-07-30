withr::local_options(
  list(tigris_use_cache = TRUE, cli.default_handler = suppressMessages),
  .local_envir = teardown_env()
)
