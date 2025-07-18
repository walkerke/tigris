test_that("places errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(places(year = 2018))
  expect_error(places(year = 2017, cb = TRUE))
  expect_error(places(year = 2009, state = "WY"))
})

test_that("places works", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))
  state <- "WY"
  expect_s3_class(places(year = 2019, cb = TRUE), "sf")
  # TODO: Enable tests if support is added for year < 2011
  # expect_s3_class(places(state = state, year = 2000, cb = TRUE), "sf")
  # expect_s3_class(places(state = state, year = 2010), "sf")

  expect_s3_class(places(state = c(state, "SD")), "sf")
  state_places <- places(state = state)
  expect_s3_class(state_places, "sf")

  expect_type(list_places(state_places), "character")
  expect_type(list_places(state_places, sorted = TRUE), "character")
})
