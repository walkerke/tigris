test_that("metro areas functions work", {
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(core_based_statistical_areas(), "sf")

  expect_s3_class(urban_areas(), "sf")

  # expect_s3_class(combined_statistical_areas(), "sf")

  expect_s3_class(metro_divisions(year = 2019), "sf")

  expect_s3_class(new_england(year = 2019), "sf")

})
