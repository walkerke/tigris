test_that("transportation functions work", {
  withr::local_options(list(tigris_use_cache = TRUE))

  state <- "WY"

  expect_s3_class(roads(state = state, county = "01"), "sf")
  expect_s3_class(roads(state = state, county = c("001", "003")), "sf")

  expect_s3_class(primary_roads(), "sf")
  expect_s3_class(primary_secondary_roads(state = state), "sf")

  expect_s3_class(rails(), "sf")

  expect_s3_class(address_ranges(state = state, county = "01"), "sf")
})
