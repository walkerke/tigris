test_that("call_geolocator works", {
  expect_identical(
    call_geolocator("3700 Koppers St", "Baltimore", "MD"),
    "245102501032024"
  )
})
