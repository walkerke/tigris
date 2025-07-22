test_that("native functions work", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(native_areas(), "sf")
  expect_s3_class(native_areas(cb = TRUE), "sf")
  # TODO: Enable test if support year < 2011 is added
  # expect_s3_class(native_areas(year = 2000), "sf")

  expect_s3_class(tribal_subdivisions_national(), "sf")
  expect_s3_class(tribal_subdivisions_national(year = 2014), "sf")
  expect_s3_class(tribal_subdivisions_national(cb = TRUE), "sf")
  # TODO: Enable if year < 2011 support is added
  # expect_s3_class(tribal_subdivisions_national(year = 2000), "sf")

  expect_s3_class(alaska_native_regional_corporations(), "sf")
  expect_s3_class(alaska_native_regional_corporations(cb = TRUE), "sf")
  # TODO: Enable if year < 2011 support is added
  # expect_s3_class(alaska_native_regional_corporations(year = 2000), "sf")

  expect_s3_class(tribal_block_groups(), "sf")
  expect_s3_class(tribal_block_groups(cb = TRUE), "sf")
  # TODO: Enable test after adding year < 2011 support
  # expect_s3_class(tribal_block_groups(year = 2010), "sf")

  expect_s3_class(tribal_census_tracts(), "sf")
  expect_s3_class(tribal_census_tracts(cb = TRUE), "sf")
  # TODO: Enable test after adding year < 2011 support
  # expect_s3_class(tribal_census_tracts(year = 2010), "sf")
})
