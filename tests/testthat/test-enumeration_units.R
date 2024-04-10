test_that("enumeration unit functions work", {
  withr::local_options(list(tigris_use_cache = TRUE))

  expect_s3_class(counties(cb = TRUE), "sf")
  expect_s3_class(counties(), "sf")

  state <- "WY"
  counties <- counties(state = state)

  expect_s3_class(counties, "sf")
  expect_s3_class(counties(state = state, cb = TRUE), "sf")

  expect_s3_class(school_districts(state = state), "sf")

  county <- "Niobrara"
  expect_s3_class(tracts(state = state, county = county), "sf")
  expect_s3_class(block_groups(state = state, county = county), "sf")
  expect_s3_class(blocks(state = state, county = county), "sf")

  expect_s3_class(zctas(state = state, year = 2010), "sf")
})
