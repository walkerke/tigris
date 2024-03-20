test_that("native functions work", {
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(native_areas(), "sf")
  expect_s3_class(native_areas(cb = TRUE), "sf")

  expect_s3_class(tribal_subdivisions_national(), "sf")
  expect_s3_class(tribal_subdivisions_national(year = 2014), "sf")
  expect_s3_class(tribal_subdivisions_national(cb = TRUE), "sf")

  expect_s3_class(alaska_native_regional_corporations(), "sf")
  expect_s3_class(alaska_native_regional_corporations(cb = TRUE), "sf")

  expect_s3_class(tribal_block_groups(), "sf")
  expect_s3_class(tribal_block_groups(cb = TRUE), "sf")

  expect_s3_class(tribal_census_tracts(), "sf")
  expect_s3_class(tribal_census_tracts(cb = TRUE), "sf")

})
