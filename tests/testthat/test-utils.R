test_that("validate_state() matches FIPS codes, names, and abbreviations", {
  expect_identical(validate_state("24"), "24")
  expect_identical(validate_state("Maryland"), "24")
  expect_identical(validate_state(c("md", "VA"), multiple = TRUE), c("24", "51"))
})

test_that("validate_state() warns or errors on invalid input", {
  expect_warning(
    expect_identical(validate_state(c("MD", "zz"), multiple = TRUE), "24"),
    "invalid"
  )
  expect_warning(expect_null(validate_state("zz")), "invalid")
  expect_error(validate_state("zz", require_state = TRUE), "zz")
})

test_that("validate_county() matches FIPS codes and names", {
  expect_identical(validate_county("MD", "5"), "005")
  expect_identical(validate_county("MD", "Anne Arundel"), "003")
  expect_identical(validate_county("MD", "St. Mary's"), "037")
})

test_that("validate_county() drops invalid and ambiguous values", {
  expect_warning(expect_null(validate_county("MD", "999")), "invalid")
  expect_warning(
    expect_null(validate_county("MD", "Baltimore")),
    "refine your selection"
  )
  expect_warning(
    expect_identical(
      validate_county("MD", c("Baltimore", "003"), multiple = TRUE),
      "003"
    ),
    "refine your selection"
  )
  expect_error(
    validate_county("MD", "Baltimore", require_county = TRUE),
    "refine your selection"
  )
})

test_that("validate_county() warns when state is missing or invalid", {
  expect_warning(
    expect_null(validate_county(NULL, "005")),
    "without a valid `state`"
  )
})
