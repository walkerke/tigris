test_that("transportation functions work", {
  skip_on_cran()
  skip_on_ci()

  state <- "WY"

  expect_s3_class(roads(state = state, county = "01"), "sf")
  expect_s3_class(roads(state = state, county = c("001", "003")), "sf")

  expect_s3_class(primary_roads(), "sf")
  # TODO: Enable if year < 2011 support is added
  # expect_s3_class(primary_roads(year = 2010), "sf")

  expect_s3_class(primary_secondary_roads(state = state), "sf")
  # TODO: Enable if year < 2011 support is added
  # expect_s3_class(primary_secondary_roads(state = state, year = 2010), "sf")

  expect_s3_class(rails(), "sf")

  expect_s3_class(address_ranges(state = state, county = "01"), "sf")
})
