test_that("nation errors", {
  expect_error(nation(resolution = "500K"))
})

test_that("national functions work", {
  withr::local_options(list(tigris_use_cache = TRUE))
  res <- "20m"

  expect_s3_class(regions(resolution = res), "sf")
  expect_s3_class(divisions(resolution = res), "sf")
  expect_s3_class(nation(resolution = "20m"), "sf")

})
