#' Download an American Indian / Alaska Native / Native Hawaiian Areas shapefile into R.
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) file.  Defaults to TRUE (the most detailed TIGER/Line file)
#' @export

native_areas <- function(detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_aiannh_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/AIANNH/tl_2014_us_aiannh.zip"

  }

  nareas <- load_tiger(url)

  nareas

}

#' Download an American Indian Tribal Subdivision National shapefile into R.
#'
#' @export

tribal_subdivisions_national <- function() {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/AITS/tl_2014_us_aitsn.zip"

  ts <- load_tiger(url)

  ts

}

#' Download an Alaska Native Regional Corporation shapefile into R.
#'
#' @export

alaska_native_regional_corporations <- function() {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/ANRC/tl_2014_02_anrc.zip"

  anrc <- load_tiger(url)

  anrc

}

#' Download a Tribal block groups shapefile into R.
#'
#' @export

tribal_block_groups <- function() {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/TBG/tl_2014_us_tbg.zip"

  tbg <- load_tiger(url)

  tbg

}

#' Download a Tribal Census tract shapefile into R.
#'
#' @export

tribal_census_tracts <- function() {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/TTRACT/tl_2014_us_ttract.zip"

  tct <- load_tiger(url)

  tct

}


