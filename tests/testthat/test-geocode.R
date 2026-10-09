test_that("call_geolocator works", {
  skip_on_cran()
  skip_on_ci()
  expect_identical(
    call_geolocator("3700 Koppers St", "Baltimore", "MD", "21227"),
    "245102501032024"
  )
  expect_identical(
    call_geolocator_latlon("39.26199475388105", "-76.67061080190281"),
    "245102501032024"
  )
})

test_that("call_geolocator_latlon works with other vintages (#151)", {
  skip_on_cran()
  skip_on_ci()
  expect_identical(
    call_geolocator_latlon(32.4, -86.5, vintage = "Census2020_Current"),
    "010010208011015"
  )
  expect_identical(
    call_geolocator_latlon(
      32.4,
      -86.5,
      benchmark = "Public_AR_Census2020",
      vintage = "Census2010_Census2020"
    ),
    "010010208011042"
  )
  expect_error(
    call_geolocator_latlon(32.4, -86.5, vintage = 2010),
    "benchmark` and `vintage"
  )
})

test_that("call_geolocator_latlon returns NA without a block GEOID", {
  mock_geographies <- function(geographies) {
    local_mocked_bindings(
      GET = function(...) structure(list(), class = "response"),
      status_code = function(...) 200L,
      stop_for_status = function(...) invisible(NULL),
      content = function(...) list(result = list(geographies = geographies)),
      .package = "httr",
      .env = parent.frame()
    )
  }

  expect_na <- function(geographies) {
    mock_geographies(geographies)
    expect_identical(
      suppressMessages(call_geolocator_latlon(32.4, -86.5)),
      NA_character_
    )
  }

  # No block layer (ACS vintages), an empty layer, and a record without GEOID
  expect_na(list(`Census Tracts` = list(list(GEOID = "01001020801"))))
  expect_na(list(`Census Blocks` = list()))
  expect_na(list(`Census Blocks` = list(list(NAME = "Block 1015"))))

  mock_geographies(list(`2020 Census Blocks` = list(list(GEOID = "010010208011015"))))
  expect_identical(call_geolocator_latlon(32.4, -86.5), "010010208011015")
})
