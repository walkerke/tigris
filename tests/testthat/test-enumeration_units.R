test_that("enumeration unit functions work", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_snapshot(counties(cb = TRUE, progress_bar = FALSE))
  expect_snapshot(counties(progress_bar = FALSE))

  state <- "WY"
  counties <- counties(state = state)

  expect_s3_class(counties, "sf")
  expect_s3_class(counties(state = state, cb = TRUE), "sf")
  # TODO: Restore tests after fixing tidyselect deprecation warning
  # expect_s3_class(counties(state = state, year = 1990, cb = TRUE), "sf")
  # expect_s3_class(counties(state = state, year = 2000, cb = TRUE), "sf")
  expect_s3_class(counties(state = state, year = 2010, cb = TRUE), "sf")
  expect_s3_class(counties(state = state, year = 2014, cb = TRUE), "sf")

  expect_s3_class(counties(state = state, year = 2010), "sf")
  expect_s3_class(counties(state = state, year = 2013), "sf")

  # expect_s3_class(school_districts(year = 2019), "sf")
  expect_snapshot(school_districts(state = state, progress_bar = FALSE))
  expect_s3_class(school_districts(state = state, cb = TRUE), "sf")

  expect_s3_class(tracts(year = 2019, cb = TRUE), "sf")

  county <- "Niobrara"
  expect_snapshot(tracts(state = state, county = county, progress_bar = FALSE))

  expect_s3_class(tracts(state = state, county = county, cb = TRUE), "sf")
  # TODO: Restore test after fixing tidyselect deprecation warning
  # expect_s3_class(
  #   tracts(state = state, county = county, year = 1990, cb = TRUE),
  #   "sf"
  # )
  expect_s3_class(
    tracts(state = state, county = county, year = 2013, cb = TRUE),
    "sf"
  )
  expect_s3_class(tracts(state = state, county = county, year = 2010), "sf")

  expect_snapshot(block_groups(
    state = state,
    county = county,
    progress_bar = FALSE
  ))

  expect_s3_class(block_groups(year = 2019, cb = TRUE), "sf")
  expect_s3_class(block_groups(state = state, year = 1990, cb = TRUE), "sf")
  expect_s3_class(block_groups(state = state, year = 2010, cb = TRUE), "sf")
  expect_s3_class(block_groups(state = state, year = 2013, cb = TRUE), "sf")
  expect_s3_class(block_groups(state = state, year = 2014, cb = TRUE), "sf")
  expect_s3_class(block_groups(state = state, year = 2010), "sf")

  expect_snapshot(blocks(state = state, county = county, progress_bar = FALSE))

  expect_snapshot(zctas(state = state, year = 2010, progress_bar = FALSE))

  expect_warning(
    zctas(year = 2000, cb = TRUE),
    "CB ZCTAs for 2000 include separate polygons for discontiguous parts."
  )
  expect_warning(
    zctas(state = state, year = 2000, cb = TRUE),
    "CB ZCTAs for 2000 include separate polygons for discontiguous parts."
  )
  expect_s3_class(zctas(year = 2010, cb = TRUE), "sf")

  expect_snapshot(county_subdivisions(
    state = state,
    county = county,
    progress_bar = FALSE
  ))
})

test_that("enumeration unit functions error", {
  skip_on_cran()
  skip_on_ci()
  state <- "WY"

  expect_error(school_districts(year = 2018))

  expect_error(block_groups(year = 2018))

  expect_error(zctas(state = state, year = 2021, cb = TRUE))
  expect_error(zctas(state = state, year = 2010, cb = TRUE))
})
