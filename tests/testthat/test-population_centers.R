test_that("population_centers() errors on invalid input", {
  expect_error(population_centers("county", "WA", year = 2015), "2000, 2010, and 2020")
  expect_error(population_centers("block group"), "state")
  expect_error(population_centers("county", "GU"), "Puerto Rico")
})

test_that("population_centers() returns NAD83 points", {
  skip_on_cran()
  ctrs <- population_centers("county", "WA", year = 2020)

  expect_s3_class(ctrs, "sf")
  expect_equal(nrow(ctrs), 39)
  expect_equal(sf::st_crs(ctrs)$epsg, 4269L)
  # Adams County, as published (not shifted from NAD 1927)
  adams <- ctrs[ctrs$COUNTYFP == "001", ]
  expect_equal(
    unname(sf::st_coordinates(adams)[1, ]),
    c(-119.031492, 46.864934)
  )
})

test_that("population_centers() reads accented names and byte order marks", {
  skip_on_cran()
  nm20 <- population_centers("county", "NM", year = 2020)
  nm10 <- population_centers("county", "NM", year = 2010)
  pr10 <- population_centers("county", "PR", year = 2010)

  expect_true(all(nm20$STATEFP == "35"))
  expect_true("Doña Ana" %in% nm20$COUNAME)
  expect_true("Doña Ana" %in% nm10$COUNAME)
  expect_true("Añasco" %in% pr10$COUNAME)
})

test_that("population_centers() keeps zero-population areas as empty points", {
  skip_on_cran()
  bgs <- population_centers("block group", "DC", year = 2000)
  empty <- bgs[sf::st_is_empty(bgs), ]

  expect_equal(nrow(empty), 1)
  expect_equal(empty$POPULATION, 0L)
})

test_that("population_centers() filters national files by state", {
  skip_on_cran()
  wa <- population_centers("state", "WA")
  dc_tracts <- population_centers("tract", "DC", year = 2000)

  expect_equal(nrow(wa), 1)
  expect_equal(wa$STNAME, "Washington")
  expect_true(nrow(dc_tracts) > 0)
  expect_true(all(dc_tracts$STATEFP == "11"))
  # The 2000 state file has no Puerto Rico row
  expect_error(population_centers("state", "PR", year = 2000), "no data")
})

test_that("population_centers() defaults to 2020 regardless of tigris_year", {
  skip_on_cran()
  withr::local_options(tigris_year = 2025)
  dc <- population_centers("county", "DC")

  expect_equal(dc$POPULATION, 689545L)
})
