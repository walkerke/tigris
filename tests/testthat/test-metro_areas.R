test_that("metro areas functions work", {
  skip_on_cran()
  skip_on_ci()

  expect_s3_class(core_based_statistical_areas(), "sf")
  expect_s3_class(core_based_statistical_areas(year = 2010), "sf")
  expect_s3_class(core_based_statistical_areas(cb = TRUE), "sf")
  expect_s3_class(core_based_statistical_areas(year = 2010, cb = TRUE), "sf")
  expect_s3_class(core_based_statistical_areas(year = 2013, cb = TRUE), "sf")

  expect_s3_class(urban_areas(), "sf")
  # TODO: Check if data is unavailable or if test fails for another reason
  # expect_s3_class(urban_areas(cb = TRUE), "sf")
  expect_s3_class(urban_areas(year = 2013, cb = TRUE), "sf")
  expect_s3_class(urban_areas(year = 2020, criteria = 2020), "sf")

  expect_s3_class(combined_statistical_areas(), "sf")
  expect_s3_class(combined_statistical_areas(cb = TRUE), "sf")
  expect_s3_class(combined_statistical_areas(cb = TRUE, year = 2013), "sf")

  expect_s3_class(metro_divisions(), "sf")

  expect_s3_class(new_england(year = 2021), "sf")
  expect_s3_class(new_england(year = 2021, cb = TRUE), "sf")
  expect_s3_class(new_england(year = 2021, type = "combined"), "sf")
  expect_s3_class(new_england(year = 2021, type = "divisions"), "sf")
})

test_that("metro areas functions error", {
  skip_on_cran()
  skip_on_ci()
  expect_error(urban_areas(year = 2020, criteria = 2020, cb = TRUE))
  expect_error(urban_areas(year = 2021, criteria = 2020))

  expect_error(combined_statistical_areas(year = 2022))

  expect_error(new_england(year = 2024))
})
