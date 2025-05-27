test_that("shift_geometry works", {
  us_states <- states(cb = TRUE, resolution = "20m")
  expect_s3_class(shift_geometry(us_states), "sf")
})
