test_that("enumeration unit functions work", {
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(counties(cb = TRUE), "sf")
  expect_s3_class(counties(), "sf")

  state <- "WY"
  counties <- counties(state = state)

  expect_s3_class(counties, "sf")
  expect_s3_class(counties(state = state, cb = TRUE), "sf")
  expect_s3_class(counties(state = state, year = 1990, cb = TRUE), "sf")
  expect_s3_class(counties(state = state, year = 2010, cb = TRUE), "sf")
  expect_s3_class(counties(state = state, year = 2010), "sf")
  expect_s3_class(counties(state = state, year = 2013), "sf")

  # expect_s3_class(school_districts(year = 2019), "sf")
  expect_s3_class(school_districts(state = state), "sf")
  expect_s3_class(school_districts(state = state, cb = TRUE), "sf")

  expect_s3_class(tracts(year = 2019, cb = TRUE), "sf")

  county <- "Niobrara"
  expect_s3_class(tracts(state = state, county = county), "sf")
  expect_s3_class(tracts(state = state, county = county, cb = TRUE), "sf")
  expect_s3_class(tracts(state = state, county = county, year = 1990, cb = TRUE), "sf")
  expect_s3_class(tracts(state = state, county = county, year = 2013, cb = TRUE), "sf")
  expect_s3_class(tracts(state = state, county = county, year = 2010), "sf")

  expect_s3_class(block_groups(state = state, county = county), "sf")
  expect_s3_class(blocks(state = state, county = county), "sf")

  expect_s3_class(zctas(state = state, year = 2010), "sf")
})

test_that("enumeration unit functions error", {
  state <- "WY"

  expect_error(school_districts(year = 2018))

  expect_error(block_groups(year = 2018))

  expect_error(zctas(state = state, year = 2021, cb = TRUE))
  expect_error(zctas(state = state, year = 2010, cb = TRUE))

})
