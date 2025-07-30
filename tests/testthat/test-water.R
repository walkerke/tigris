test_that("water functions error", {
  skip_on_cran()
  skip_on_ci()
  state <- "NOH"
  expect_error(
    suppressWarnings(
      area_water(state, county = c("011", "015"))
    )
  )
  expect_error(
    suppressWarnings(
      linear_water(state, county = "011")
    )
  )
})

test_that("water functions work", {
  skip_on_cran()
  skip_on_ci()

  state <- "NH"

  expect_s3_class(area_water(state, county = "011"), "sf")
  expect_s3_class(area_water(state, county = c("011", "015")), "sf")
  # TODO: Enable if year < 2011 support is added
  # expect_s3_class(area_water(state, county = "011", year = 2010), "sf")

  expect_s3_class(linear_water(state, county = "011"), "sf")
  expect_s3_class(linear_water(state, county = c("011", "015")), "sf")
  # TODO: Enable if year < 2011 support is added
  # expect_s3_class(linear_water(state, county = "011", year = 2010), "sf")

  expect_s3_class(coastline(), "sf")
  expect_s3_class(coastline(year = 2016), "sf")
})
