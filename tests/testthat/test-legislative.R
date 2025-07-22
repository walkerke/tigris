test_that("congressional_districts warns and errors", {
  skip_on_cran()
  skip_on_ci()
  # TODO: Enable if warning for invalid states is added
  # expect_warning(
  #   congressional_districts(state = "WYO", year = 2010),
  #   ".no valid FIPS codes or state name/abbreviations and must be ignored"
  # )

  expect_error(congressional_districts(year = 2009))
  expect_error(congressional_districts(year = 2012, cb = TRUE))
  expect_error(congressional_districts(
    year = 2013,
    cb = TRUE,
    resolution = "2m"
  ))
})


test_that("congressional_districts works", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(congressional_districts(), "sf")
  # TODO: Check why test failed on CI
  # expect_s3_class(congressional_districts(year = 2010), "sf")

  cb <- TRUE
  res <- "20m"
  state <- "WY"

  expect_s3_class(
    congressional_districts(year = 2022, resolution = "20m", cb = TRUE),
    "sf"
  )
  expect_s3_class(
    congressional_districts(
      state = state,
      year = 2022,
      resolution = res,
      cb = cb
    ),
    "sf"
  )
  expect_s3_class(
    congressional_districts(
      state = state,
      year = 2018,
      resolution = res,
      cb = cb
    ),
    "sf"
  )
  expect_s3_class(
    congressional_districts(
      state = state,
      year = 2016,
      resolution = res,
      cb = cb
    ),
    "sf"
  )
  expect_s3_class(
    congressional_districts(
      state = state,
      year = 2014,
      resolution = res,
      cb = cb
    ),
    "sf"
  )
  expect_s3_class(
    congressional_districts(
      state = state,
      year = 2013,
      resolution = res,
      cb = cb
    ),
    "sf"
  )
  expect_s3_class(
    congressional_districts(
      state = state,
      year = 2011,
      resolution = res,
      cb = FALSE
    ),
    "sf"
  )
  expect_s3_class(
    congressional_districts(
      state = state,
      year = 2010,
      resolution = res,
      cb = FALSE
    ),
    "sf"
  )
})


test_that("state_legislative_districts errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(state_legislative_districts())
  expect_error(state_legislative_districts(house = "uppr"))
})


test_that("state_legislative_districts works", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))

  state <- "WY"

  expect_s3_class(state_legislative_districts(cb = TRUE, year = 2019, ), "sf")
  expect_s3_class(
    state_legislative_districts(state = state, house = "upper"),
    "sf"
  )
  expect_s3_class(
    state_legislative_districts(state = state, house = "lower"),
    "sf"
  )
  expect_s3_class(
    state_legislative_districts(state = 31, house = "lower"),
    "sf"
  )
  expect_s3_class(
    state_legislative_districts(state = state, house = "upper"),
    "sf"
  )
  expect_s3_class(
    state_legislative_districts(
      state = state,
      house = "upper",
      year = 2010
    ),
    "sf"
  )
  expect_s3_class(
    state_legislative_districts(
      state = state,
      house = "upper",
      cb = TRUE,
      year = 2013
    ),
    "sf"
  )
  # TODO: Check if this test is supposed to work or not
  # expect_s3_class(
  #   state_legislative_districts(
  #     state = state,
  #     house = "upper",
  #     year = 2010,
  #     cb = TRUE
  #   ),
  #   "sf"
  # )
  # expect_s3_class(
  #   state_legislative_districts(
  #     state = state,
  #     house = "lower",
  #     year = 2010,
  #     cb = TRUE
  #   ),
  #   "sf"
  # )
})


test_that("voting_districts errors", {
  skip_on_cran()
  skip_on_ci()
  expect_error(voting_districts())
  expect_error(voting_districts(year = 2018, cb = TRUE))
  expect_error(voting_districts(year = 2018))
})


test_that("voting_districts works", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(voting_districts(cb = TRUE), "sf")
  state <- "WY"

  expect_s3_class(voting_districts(state = state, cb = TRUE), "sf")
  expect_s3_class(voting_districts(state = state, county = "Albany"), "sf")
  expect_s3_class(
    voting_districts(state = state, county = "Albany", cb = TRUE),
    "sf"
  )
  expect_s3_class(voting_districts(state = state, year = 2012), "sf")
})
