test_that("rbind_tigris() matches sequential rbind", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  # Pieces with default row names, like downloaded tigris objects
  pieces <- lapply(split(nc, rep(1:5, length.out = nrow(nc))), function(x) {
    rownames(x) <- NULL
    attr(x, "tigris") <- "county"
    x
  })

  expected <- Reduce(rbind, pieces)
  attr(expected, "tigris") <- "county"

  expect_identical(rbind_tigris(pieces), expected)
})
