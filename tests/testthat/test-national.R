test_that("nation errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(nation(resolution = "500K"))
})

test_that("national functions work", {
  skip_on_cran()
  skip_on_ci()
  res <- "20m"

  expect_s3_class(regions(resolution = res), "sf")
  expect_s3_class(divisions(resolution = res), "sf")
  expect_s3_class(nation(resolution = res), "sf")
})
