test_that("places errors", {
  expect_error(places(year = 2018))
  expect_error(places(year = 2017, cb = TRUE))
})

test_that("places works", {
  withr::local_options(list(tigris_use_cache = TRUE))
  state <- "WY"
  expect_s3_class(places(year = 2019, cb = TRUE), "sf")
  expect_s3_class(places(state = c(state, "SD")), "sf")

  state_places <- places(state = state)
  expect_s3_class(state_places, "sf")

  expect_type(list_places(state_places), "character")
  expect_type(list_places(state_places, sorted = TRUE), "character")
})
