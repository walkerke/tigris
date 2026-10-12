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

test_that("validate_county() drops invalid values", {
  expect_warning(expect_null(validate_county("MD", "999")), "invalid")
  expect_warning(
    expect_identical(
      validate_county("MD", c("zz", "003"), multiple = TRUE),
      "003"
    ),
    "can't be matched"
  )
  expect_error(
    validate_county("MD", "zz", require_county = TRUE),
    "can't be matched"
  )
})

test_that("validate_county() prefers a complete county name", {
  expect_identical(validate_county("TX", "Collin"), "085")
  expect_identical(validate_county("TX", "Collingsworth"), "087")
  expect_identical(validate_county("IA", "clay"), "041")
  expect_identical(validate_county("MD", "Baltimore County"), "005")
  expect_identical(validate_county("MD", "Baltimore city"), "510")
})

test_that("validate_county() errors on ambiguous names outside interactive sessions", {
  withr::local_options(rlang_interactive = FALSE)

  expect_error(
    validate_county("MD", "Baltimore"),
    "Baltimore County \\(005\\) and Baltimore city \\(510\\)"
  )
  expect_error(validate_county("TX", "Coll"), "Collingsworth")
})

test_that("validate_county() asks about ambiguous names in interactive sessions", {
  withr::local_options(rlang_interactive = TRUE)

  local_mocked_bindings(county_menu = function(choices, title) {
    expect_match(title, "Which county are you requesting")
    expect_identical(choices, c("Baltimore County (005)", "Baltimore city (510)"))
    2L
  })
  expect_identical(validate_county("MD", "Baltimore", .msg = FALSE), "510")

  local_mocked_bindings(county_menu = function(choices, title) 0L)
  expect_error(validate_county("MD", "Baltimore", .msg = FALSE), "No county selected")
})

test_that("validate_county() warns when state is missing or invalid", {
  expect_warning(
    expect_null(validate_county(NULL, "005")),
    "without a valid `state`"
  )
})
