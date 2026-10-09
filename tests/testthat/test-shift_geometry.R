test_that("shift_geometry works", {
  skip_on_cran()
  skip_on_ci()
  us_states <- states(cb = TRUE, resolution = "20m")
  expect_s3_class(shift_geometry(us_states), "sf")
})

test_that("shift_geometry handles the Hawaiian archipelago and multi-state features", {
  skip_on_cran()
  skip_on_ci()

  # Northwestern Hawaiian Islands tract is dropped, not left in place (#198)
  hi_tracts <- tracts(state = "HI", cb = TRUE, year = 2024)
  shifted <- shift_geometry(hi_tracts)
  expect_false("9812" %in% shifted$NAME)
  expect_equal(nrow(shifted), nrow(hi_tracts) - 1)
  expect_equal(
    sf::st_geometry(shifted),
    sf::st_geometry(shift_geometry(hi_tracts, geoid_column = "GEOID"))
  )

  # The West region is split, shifted, and recombined (#219)
  us_regions <- regions(year = 2024)
  shifted_regions <- shift_geometry(us_regions)
  expect_equal(shifted_regions$NAME, us_regions$NAME)
  expect_true(all(sf::st_geometry_type(shifted_regions) == "MULTIPOLYGON"))
  # Same footprint as shifted states (unshifted, the West's Alaska/Hawaii parts
  # are millions of meters away)
  shifted_states <- states(cb = TRUE, resolution = "20m", year = 2024) %>%
    shift_geometry()
  expect_lt(
    max(abs(sf::st_bbox(shifted_regions) - sf::st_bbox(shifted_states))),
    50000
  )
})
