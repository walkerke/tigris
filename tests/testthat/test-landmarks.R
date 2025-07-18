test_that("landmarks errors", {
  expect_error(landmarks(state = "WY", type = "pont"))
})

test_that("landmarks works", {
  withr::local_options(list(tigris_use_cache = TRUE))

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
  withr::local_options(list(tigris_use_cache = TRUE))
  expect_snapshot(military(progress_bar = FALSE))
})
