test_that("enumeration unit functions work", {
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(counties(cb = TRUE), "sf")
  expect_s3_class(counties(), "sf")

  state <- "WY"
  expect_s3_class(counties(state = state), "sf")
  expect_s3_class(counties(state = state, cb = TRUE), "sf")

  expect_s3_class(zctas(state = state, year = 2010), "sf")
})
