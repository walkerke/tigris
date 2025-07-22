test_that("`counties()` works", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_snapshot(counties(cb = TRUE, progress_bar = FALSE))
  expect_snapshot(counties(progress_bar = FALSE))

  state <- "WY"
  counties <- counties(state = state)

  expect_s3_class(counties, "sf")
  expect_s3_class(counties(state = state, year = 2010), "sf")
  expect_s3_class(counties(state = state, year = 2013), "sf")
  expect_s3_class(counties(state = state, cb = TRUE), "sf")
  expect_s3_class(counties(state = state, year = 1990, cb = TRUE), "sf")
  expect_s3_class(counties(state = state, year = 2010, cb = TRUE), "sf")
  expect_s3_class(counties(state = state, year = 2013, cb = TRUE), "sf")
  expect_s3_class(counties(state = state, year = 2014, cb = TRUE), "sf")

  expect_error(
    counties(state = state, year = 1990)
  )
})

test_that("`school_districts()` works", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))
  state <- "WY"

  expect_snapshot(school_districts(state = state, progress_bar = FALSE))
  expect_s3_class(school_districts(state = state, cb = TRUE), "sf")
  expect_s3_class(school_districts(year = 2019, cb = TRUE), "sf")
  expect_error(school_districts(year = 2018))
})

test_that("`tracts()` works", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))
  state <- "WY"
  county <- "Niobrara"
  expect_snapshot(tracts(state = state, county = county, progress_bar = FALSE))

  expect_s3_class(tracts(state = state, county = county, cb = TRUE), "sf")
  expect_s3_class(
    tracts(state = state, county = county, year = 1990, cb = TRUE),
    "sf"
  )
  expect_s3_class(
    tracts(state = state, county = county, year = 2013, cb = TRUE),
    "sf"
  )
  expect_s3_class(tracts(state = state, county = county, year = 2010), "sf")

  expect_s3_class(
    tracts(state = state, year = 1990, cb = TRUE),
    "sf"
  )

  expect_s3_class(
    tracts(state = state, year = 2010, cb = TRUE),
    "sf"
  )

  expect_s3_class(tracts(year = 2019, cb = TRUE), "sf")

  expect_s3_class(tracts(state = state, year = 2019, cb = TRUE), "sf")

  expect_s3_class(tracts(state = state, year = 2013, cb = TRUE), "sf")

  expect_error(
    tracts(state = state, resolution = "5m")
  )

  expect_error(
    tracts(state = state, year = 1990)
  )
})

test_that("`block_groups()` work", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))
  state <- "WY"
  county <- "Niobrara"

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

  expect_error(block_groups(year = 2018))
})

test_that("`blocks()` work", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))
  state <- "WY"
  county <- "Niobrara"

  expect_snapshot(blocks(state = state, county = county, progress_bar = FALSE))

  expect_s3_class(
    blocks(state = state, county = county, year = 2020),
    "sf"
  )

  expect_s3_class(
    blocks(state = state, county = county, year = 2014),
    "sf"
  )

  expect_s3_class(
    blocks(state = state, county = county, year = 2013),
    "sf"
  )
  expect_s3_class(
    blocks(state = state, county = c(county, "Lincoln"), year = 2010),
    "sf"
  )
  expect_s3_class(
    blocks(state = state, county = c(county, "Lincoln"), year = 2011),
    "sf"
  )
  expect_s3_class(
    blocks(state = state, county = c(county, "Lincoln"), year = 2020),
    "sf"
  )
  expect_s3_class(
    blocks(state = state, year = 2000),
    "sf"
  )

  expect_error(blocks(state = state, year = 1990))
})


test_that("`zctas()` works", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))
  state <- "WY"
  county <- "Niobrara"

  expect_snapshot(zctas(state = state, year = 2010, progress_bar = FALSE))

  expect_s3_class(zctas(year = 2010, cb = TRUE), "sf")

  expect_warning(
    zctas(year = 2000, cb = TRUE),
    "CB ZCTAs for 2000 include separate polygons for discontiguous parts."
  )
  expect_warning(
    zctas(state = state, year = 2000, cb = TRUE),
    "CB ZCTAs for 2000 include separate polygons for discontiguous parts."
  )

  expect_error(zctas(state = state, year = 2021, cb = TRUE))
  expect_error(zctas(state = state, year = 2010, cb = TRUE))
})


test_that("`county_subdivisions()` works", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(tigris_use_cache = TRUE))
  state <- "WY"
  county <- "Niobrara"

  expect_snapshot(county_subdivisions(
    state = state,
    county = county,
    progress_bar = FALSE
  ))

  expect_s3_class(
    county_subdivisions(state = state, year = 2010, cb = TRUE),
    "sf"
  )

  expect_s3_class(
    county_subdivisions(state = state, year = 2013, cb = TRUE),
    "sf"
  )

  expect_s3_class(
    county_subdivisions(state = state, year = 2020, cb = TRUE),
    "sf"
  )

  expect_s3_class(county_subdivisions(state = state, year = 2010), "sf")

  expect_s3_class(county_subdivisions(state = state, year = 2020), "sf")

  expect_s3_class(
    county_subdivisions(state = state, county = county, year = 2020),
    "sf"
  )
})
