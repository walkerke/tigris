local_nc_dsn <- function(env = parent.frame()) {
  dsn <- withr::local_tempdir(.local_envir = env)
  nc <- sf::st_transform(
    sf::read_sf(system.file("shape/nc.shp", package = "sf")),
    4269
  )
  sf::write_sf(nc, file.path(dsn, "nc.shp"))
  dsn
}

test_that("tiger_read() keeps EPSG:4269 by default", {
  nc <- tiger_read(local_nc_dsn(), "nc")

  expect_s3_class(nc, "sf")
  expect_equal(nrow(nc), 100)
  expect_equal(sf::st_crs(nc)$epsg, 4269)
})

test_that("tiger_read() transforms to crs", {
  nc <- tiger_read(local_nc_dsn(), "nc", crs = 3857)

  expect_equal(sf::st_crs(nc), sf::st_crs(3857))
})

test_that("tiger_read() accepts an equivalent crs without changing it", {
  dsn <- local_nc_dsn()

  expect_equal(sf::st_crs(tiger_read(dsn, "nc", crs = 4269))$epsg, 4269)
  expect_equal(sf::st_crs(tiger_read(dsn, "nc", crs = sf::st_crs(4269)))$epsg, 4269)
})

test_that("tiger_read() ignores NULL and NA crs", {
  dsn <- local_nc_dsn()

  expect_equal(sf::st_crs(tiger_read(dsn, "nc", crs = NULL))$epsg, 4269)
  expect_equal(sf::st_crs(tiger_read(dsn, "nc", crs = NA))$epsg, 4269)
})

test_that("tiger_read() uses the tigris_crs option", {
  withr::local_options(tigris_crs = "EPSG:3857")
  nc <- tiger_read(local_nc_dsn(), "nc")

  expect_equal(sf::st_crs(nc), sf::st_crs(3857))
})

test_that("tiger_read() passes query to sf::st_read()", {
  nc <- expect_no_warning(
    tiger_read(
      local_nc_dsn(),
      "nc",
      query = "SELECT * FROM {layer} WHERE NAME = 'Ashe'"
    )
  )

  expect_equal(nrow(nc), 1)
  expect_equal(nc$NAME, "Ashe")
})

test_that("tiger_read() combines query with wkt_filter", {
  dsn <- local_nc_dsn()
  nc <- sf::read_sf(file.path(dsn, "nc.shp"))
  wkt <- input_to_wkt(sf::st_bbox(nc[nc$NAME == "Ashe", ]))
  res <- tiger_read(
    dsn,
    "nc",
    query = "SELECT * FROM {layer} WHERE NAME IN ('Ashe', 'Dare')",
    wkt_filter = wkt
  )

  expect_equal(res$NAME, "Ashe")
})

test_that("load_tiger() accepts crs", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  expect_equal(
    sf::st_crs(states(cb = TRUE, resolution = "20m", crs = 3857)),
    sf::st_crs(3857)
  )
})

test_that("as_tigris_query() substitutes the layer name", {
  expect_identical(
    as_tigris_query("SELECT * FROM {layer} WHERE STATEFP = '24'", "tl_2025_us_county"),
    "SELECT * FROM \"tl_2025_us_county\" WHERE STATEFP = '24'"
  )
  expect_identical(as_tigris_query("SELECT * FROM nc", "nc"), "SELECT * FROM nc")
  expect_identical(as_tigris_query(NA, "nc"), NA_character_)
  expect_identical(as_tigris_query(NULL, "nc"), NA_character_)
  expect_error(as_tigris_query(1, "nc"), "query")
})

test_that("input_to_wkt() repairs invalid geometries using geos_method", {
  # geos_method is ignored for geodetic coordinates when S2 is enabled
  s2 <- sf::sf_use_s2(FALSE)
  withr::defer(sf::sf_use_s2(s2))

  bowtie <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0)))),
    crs = 4269
  )

  expect_false(sf::st_is_valid(bowtie))
  expect_match(input_to_wkt(bowtie), "^POLYGON|^MULTIPOLYGON")
  expect_match(
    input_to_wkt(bowtie, geos_method = "valid_linework"),
    "^POLYGON|^MULTIPOLYGON"
  )
})
