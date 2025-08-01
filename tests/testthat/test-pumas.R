test_that("pumas works", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))
  expect_s3_class(pumas(year = 2020, cb = TRUE), "sf")

  state <- "WY"
  # TODO: Enable test if support is added
  # expect_s3_class(pumas(state = state), "sf")
  expect_s3_class(pumas(state = state, year = 2013), "sf")
  expect_s3_class(pumas(year = 2019, cb = TRUE, state = state), "sf")
  expect_s3_class(pumas(year = 2013, cb = TRUE, state = state), "sf")
})

test_that("pumas errors", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_error(pumas(year = 2018))
  expect_error(pumas(year = 2021, cb = TRUE))
  expect_error(pumas(year = 2021, state = "WY", cb = TRUE))
})
