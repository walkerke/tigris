test_that("landmarks errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(landmarks(state = "WY", type = "pont"))
})

test_that("landmarks works", {
  skip_on_cran()
  skip_on_ci()

  state <- "WY"
  area_landmarks <- landmarks(
    state = state,
    type = "area",
    progress_bar = FALSE
  )
  point_landmarks <- landmarks(
    state = state,
    type = "point",
    progress_bar = FALSE
  )

  expect_snapshot(area_landmarks)
  expect_snapshot(point_landmarks)
})

test_that("military works", {
  skip_on_cran()
  skip_on_ci()
  expect_snapshot(military(progress_bar = FALSE))
})
