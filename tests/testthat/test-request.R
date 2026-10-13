# Offline tests of the download error messages and staged cache updates.
# Mocked responses bypass req_retry(); dev/httr2/retry_check.R checks the real
# retry loop against a local server.

make_layer_zip <- function(dir, rows, files = c("shp", "shx", "dbf", "prj"), flags = "-q") {
  src <- tempfile("src-", tmpdir = dir)
  dir.create(src)
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  sf::st_write(nc[rows, ], file.path(src, "layer.shp"), quiet = TRUE)
  zip <- tempfile("layer-", tmpdir = dir, fileext = ".zip")
  withr::with_dir(src, utils::zip(zip, paste0("layer.", files), flags = flags))
  zip
}

test_that("tigris_download() explains missing files and server errors", {
  path <- withr::local_tempfile(fileext = ".zip")

  httr2::local_mocked_responses(function(req) httr2::response(404))
  expect_error(
    tigris_download("https://www2.census.gov/x.zip", path),
    class = "tigris_file_not_found"
  )

  httr2::local_mocked_responses(function(req) httr2::response(503))
  expect_error(
    tigris_download("https://www2.census.gov/x.zip", path),
    "503 Service Unavailable"
  )
  expect_false(file.exists(path))
})

test_that("tiger_fetch() doesn't fall back to FTP for a missing file", {
  tried <- character()
  local_mocked_bindings(tigris_download = function(url, ...) {
    tried <<- c(tried, url)
    stop_file_not_found(url)
  })

  expect_error(
    tiger_fetch("https://www2.census.gov/x.zip", "ftp://ftp2.census.gov/x.zip",
                tempfile(), "x", withr::local_tempdir(), FALSE),
    class = "tigris_file_not_found"
  )
  expect_identical(tried, "https://www2.census.gov/x.zip")
})

test_that("tiger_fetch() falls back to FTP for other failures", {
  # setup.R silences cli messages for the suite
  withr::local_options(cli.default_handler = NULL)
  local_mocked_bindings(tigris_download = function(url, path, ...) {
    if (startsWith(url, "https")) {
      cli::cli_abort("Unable to download", class = "tigris_download_error")
    }
    invisible(path)
  })

  expect_message(
    tiger_fetch("https://www2.census.gov/x.zip", "ftp://ftp2.census.gov/x.zip",
                tempfile(), "x", withr::local_tempdir(), FALSE),
    "trying FTP as fallback"
  )
})

test_that("extract_layer() accepts a complete layer", {
  dir <- withr::local_tempdir()
  out <- file.path(dir, "out")
  dir.create(out)

  expect_true(extract_layer(make_layer_zip(dir, 1:3), "layer", out))
})

test_that("extract_layer() rejects incomplete, truncated, and corrupt files", {
  dir <- withr::local_tempdir()
  check <- function(zip) {
    out <- tempfile("out-", tmpdir = dir)
    dir.create(out)
    extract_layer(zip, "layer", out)
  }

  # No .dbf
  expect_false(check(make_layer_zip(dir, 1:3, files = c("shp", "shx"))))

  # All files present, but the .shp is cut off after its header
  src <- tempfile("src-", tmpdir = dir)
  dir.create(src)
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  sf::st_write(nc[1:3, ], file.path(src, "layer.shp"), quiet = TRUE)
  shp <- file.path(src, "layer.shp")
  writeBin(readBin(shp, "raw", n = 100), shp)
  truncated <- file.path(dir, "truncated.zip")
  withr::with_dir(src, utils::zip(truncated, list.files(), flags = "-q"))
  expect_false(check(truncated))

  # Damaged compressed data (the zip's directory is intact)
  corrupt <- make_layer_zip(dir, 1:100)
  bytes <- readBin(corrupt, "raw", file.size(corrupt))
  i <- length(bytes) %/% 3
  bytes[i] <- xor(bytes[i], as.raw(0xff))
  writeBin(bytes, corrupt)
  expect_false(check(corrupt))

  # Uncompressed, so only the checksum shows the damage
  stored <- make_layer_zip(dir, 1:100, flags = "-q0")
  bytes <- readBin(stored, "raw", file.size(stored))
  i <- length(bytes) %/% 2
  bytes[i] <- xor(bytes[i], as.raw(0xff))
  writeBin(bytes, stored)
  expect_false(check(stored))
})

test_that("publish_layer() replaces a cached layer's files", {
  dir <- withr::local_tempdir()
  cache <- file.path(dir, "cache")
  dir.create(cache)
  staged <- function(rows, files = c("shp", "shx", "dbf", "prj")) {
    out <- tempfile("staged-", tmpdir = dir)
    dir.create(out)
    stopifnot(extract_layer(make_layer_zip(dir, rows, files), "layer", out))
    out
  }

  publish_layer(staged(1:3), "layer", cache)
  # A stale file from an older download that the new one doesn't have
  writeLines("UTF-8", file.path(cache, "layer.cpg"))
  publish_layer(staged(4:6, c("shp", "shx", "dbf")), "layer", cache)

  expect_setequal(list.files(cache), c("layer.shp", "layer.shx", "layer.dbf"))
  expect_equal(nrow(sf::st_read(file.path(cache, "layer.shp"), quiet = TRUE)), 3)
})

test_that("publish_layer() restores the old files if a move fails", {
  dir <- withr::local_tempdir()
  cache <- file.path(dir, "cache")
  dir.create(cache)
  staged <- function(rows) {
    out <- tempfile("staged-", tmpdir = dir)
    dir.create(out)
    stopifnot(extract_layer(make_layer_zip(dir, rows), "layer", out))
    out
  }
  publish_layer(staged(1:3), "layer", cache)
  old <- sf::st_read(file.path(cache, "layer.shp"), quiet = TRUE)
  new <- staged(4:6)

  local_mocked_bindings(move_file = function(from, to) {
    if (startsWith(from, new) && basename(from) == "layer.dbf") return(FALSE)
    file.rename(from, to)
  })

  expect_error(publish_layer(new, "layer", cache), "previous files were kept")
  expect_identical(sf::st_read(file.path(cache, "layer.shp"), quiet = TRUE), old)
  expect_length(list.files(cache, pattern = "^backup-"), 0)
})
