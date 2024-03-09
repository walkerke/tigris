test_that("enumeration unit functions work", {
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(counties(cb = TRUE), "sf")
  expect_s3_class(counties(), "sf")

})
