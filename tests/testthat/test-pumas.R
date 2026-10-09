test_that("pumas works", {
  skip_on_cran()
  skip_on_ci()
  expect_s3_class(pumas(year = 2020, cb = TRUE), "sf")

  state <- "WY"
  expect_s3_class(pumas(state = state), "sf")
  # 2020 PUMAs are in PUMA/ through 2023 and PUMA20/ from 2024 (#213)
  expect_s3_class(pumas(state = state, year = 2023), "sf")
  expect_s3_class(pumas(state = state, year = 2025), "sf")
  expect_s3_class(pumas(state = state, year = 2013), "sf")
  expect_s3_class(pumas(year = 2019, cb = TRUE, state = state), "sf")
  expect_s3_class(pumas(year = 2013, cb = TRUE, state = state), "sf")
})

test_that("pumas errors", {
  skip_on_cran()
  skip_on_ci()

  expect_error(pumas(year = 2018))
  expect_error(pumas(year = 2021, cb = TRUE))
  expect_error(pumas(year = 2021, state = "WY", cb = TRUE))
})
