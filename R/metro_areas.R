#' Download a core-based statistical area shapefile into R
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) cartographic boundary file.  Defaults
#' to TRUE (the most detailed TIGER/Line file).
#' @export

core_based_statistical_areas <- function(detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_cbsa_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/CBSA/tl_2014_us_cbsa.zip"

  }

  cbsa <- load_tiger(url)

  cbsa

}

#' Download an urbanized areas shapefile into R
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) cartographic boundary file.  Defaults
#' to TRUE (the most detailed TIGER/Line file).
#' @export

urban_areas <- function(detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_ua10_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/UAC/tl_2014_us_uac10.zip"

  }

  urban <- load_tiger(url)

  urban

}

#' Download a combined statistical areas shapefile into R
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) cartographic boundary file.  Defaults
#' to TRUE (the most detailed TIGER/Line file).
#' @export

combined_statistical_areas <- function(detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_csa_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/CSA/tl_2014_us_csa.zip"

  }

  csa <- load_tiger(url)

  csa

}

#' Download a metropolitan divisions shapefile into R.
#'
#' @export

metro_divisions <- function() {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/METDIV/tl_2014_us_metdiv.zip"

  md <- load_tiger(url)

  md

}
