test_that("states works", {
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(states(year = 1990, cb = TRUE), "sf")
  expect_s3_class(states(year = 2010, cb = TRUE), "sf")
  expect_s3_class(states(year = 2014, cb = TRUE), "sf")
  # expect_s3_class(states(year = 2013, cb = TRUE), "sf")

  expect_s3_class(states(year = 2010), "sf")
  expect_s3_class(states(year = 2012), "sf")
  expect_s3_class(states(), "sf")
})
