test_that("water functions error", {
  state <- "NOH"
  expect_error(area_water(state, county = c("011", "015")))
  expect_error(linear_water(state, county = "011"))
})

test_that("water functions work", {
  withr::local_options(list(tigris_use_cache = TRUE))

  state <- "NH"

  expect_s3_class(area_water(state, county = "011"), "sf")
  expect_s3_class(area_water(state, county = c("011", "015")), "sf")
  expect_s3_class(area_water(state, county = "011", year = 2010), "sf")

  expect_s3_class(linear_water(state, county = "011"), "sf")
  expect_s3_class(linear_water(state, county = c("011", "015")), "sf")
  expect_s3_class(linear_water(state, county = "011", year = 2010), "sf")

  expect_s3_class(coastline(), "sf")
  expect_s3_class(coastline(year = 2016), "sf")
})
