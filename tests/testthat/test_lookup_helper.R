context("Test State/County FIPS lookups")

test_that("State lookup works", {
  expect_that(lookup_code("me"), equals("The code for Maine is '23'."))
  expect_that(lookup_code("Maine"), equals("The code for Maine is '23'."))
  expect_that(lookup_code("23"), equals("The code for Maine is '23'."))
  expect_that(lookup_code(23), equals("The code for Maine is '23'."))
})

test_that("State + County lookup works", {
  expect_that(lookup_code("me", "york"), equals("The code for Maine is '23' and the code for York County is '031'."))
  expect_that(lookup_code("Maine", "York County"), equals("The code for Maine is '23' and the code for York County is '031'."))
})
