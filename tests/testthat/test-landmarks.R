test_that("landmarks errors", {
  expect_error(landmarks(state = "WY", type = "pont"))
})

test_that("landmarks works", {
  withr::local_options(list(tigris_use_cache = TRUE))
  state <- "WY"
  expect_s3_class(landmarks(state = state, type = "point"), "sf")
})

test_that("military works", {
  withr::local_options(list(tigris_use_cache = TRUE))
  expect_s3_class(military(), "sf")
})
