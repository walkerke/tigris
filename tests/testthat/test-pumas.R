test_that("pumas works", {
  withr::local_options(list(tigris_use_cache = TRUE))
  expect_s3_class(pumas(year = 2020, cb = TRUE), "sf")

  state <- "WY"
  expect_s3_class(pumas(state = state), "sf")
  expect_s3_class(pumas(state = state, year = 2013), "sf")
})

test_that("pumas errors", {
  expect_error(pumas(year = 2018))
  expect_error(pumas(year = 2021, cb = TRUE))
})
