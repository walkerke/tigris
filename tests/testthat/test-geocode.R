test_that("call_geolocator works", {
  expect_identical(
    call_geolocator("3700 Koppers St", "Baltimore", "MD", "21227"),
    "245102501032024"
  )
  expect_identical(
    call_geolocator_latlon("39.26199475388105", "-76.67061080190281"),
    "245102501032024"
  )
})
