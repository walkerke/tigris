test_that("State lookup works", {
  expect_equal(lookup_code("me"), "The code for Maine is '23'.")
  expect_equal(lookup_code("Maine"), "The code for Maine is '23'.")
  expect_equal(lookup_code("23"), "The code for Maine is '23'.")
  expect_equal(lookup_code(23), "The code for Maine is '23'.")
})

test_that("State + County lookup works", {
  expect_equal(lookup_code("me", "york"), "The code for Maine is '23' and the code for York County is '031'.")
  expect_equal(lookup_code("Maine", "York County"), "The code for Maine is '23' and the code for York County is '031'.")
})
