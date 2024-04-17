test_that("states works", {
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(states(year = 1990, cb = TRUE), "sf")
  expect_s3_class(states(year = 2010, cb = TRUE), "sf")
  expect_s3_class(states(year = 2014, cb = TRUE), "sf")
  expect_s3_class(states(year = 2013, cb = TRUE), "sf")

  expect_s3_class(states(year = 2010), "sf")
  expect_s3_class(states(year = 2012), "sf")

  tigris_states <- states()

  expect_s3_class(tigris_states, "sf")

  expect_snapshot(list_states(tigris_states))

  expect_snapshot(list_states(tigris_states, sorted = FALSE))

  expect_snapshot(filter_state(tigris_states, "Wyoming"))

  expect_snapshot(grep_state(tigris_states, "north"))
})
