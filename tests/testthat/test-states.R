test_that("states works", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(states(year = 1990, cb = TRUE), "sf")
  expect_s3_class(states(year = 2000, cb = TRUE), "sf")
  expect_s3_class(states(year = 2010, cb = TRUE), "sf")
  expect_s3_class(states(year = 2014, cb = TRUE), "sf")
  # TODO: Check why test failed on CI
  # expect_s3_class(states(year = 2013, cb = TRUE), "sf")

  expect_s3_class(states(year = 2010), "sf")
  expect_s3_class(states(year = 2012), "sf")

  tigris_states <- states()

  expect_s3_class(tigris_states, "sf")

  # TODO: Enable tests after adding support for sf objects
  # expect_snapshot(list_states(tigris_states))

  # expect_snapshot(list_states(tigris_states, sorted = FALSE))

  # expect_snapshot(filter_state(tigris_states, "Wyoming"))

  # expect_snapshot(grep_state(tigris_states, "north"))
})
