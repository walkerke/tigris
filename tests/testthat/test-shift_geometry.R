test_that("shift_geometry works", {
  skip_on_cran()
  skip_on_ci()
  us_states <- states(cb = TRUE, resolution = "20m")
  expect_s3_class(shift_geometry(us_states), "sf")
})
