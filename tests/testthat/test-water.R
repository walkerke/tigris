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

test_that("erase_water works", {
  skip_on_cran()
  skip_on_ci()

  dc_tracts <- tracts("DC", year = 2020)
  expect_s3_class(erase_water(dc_tracts, year = 2020), "sf")

  # No overlapping water returns the input unmodified (#172)
  dc_water <- area_water("DC", "001", year = 2020)
  dry_tracts <- dc_tracts[lengths(sf::st_intersects(dc_tracts, dc_water)) == 0, ]
  expect_identical(erase_water(dry_tracts, year = 2020), dry_tracts)

  # No cartographic boundary counties for 2011-2012; area water starts in 2011 (#182)
  expect_s3_class(erase_water(dc_tracts, year = 2012), "sf")
  expect_error(erase_water(dc_tracts, year = 2010), "2011 or later")
})
