#' Download a congressional districts shapefile for the 114th Congress into R
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) cartographic boundary file.  Defaults
#' to TRUE (the most detailed TIGER/Line file).
#' @export

congressional_districts <- function(detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_cd114_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/CD/tl_2014_us_cd114.zip"

  }

  cd <- load_tiger(url)

  cd

}

#' Download a state legislative districts shapefile into R - upper or lower
#'
#' This function allows you to download boundaries for state legislatures into R.  Generally, state legislatures
#' are comprised of an "upper" house, which is typically referred to as the Senate, and a "lower" house, which is
#' often (but not exclusively) referred to as the House.  The exception is Nebraska, which has a unicameral
#' state legislature.
#'
#' @param state The two-digit FIPS code (string) of the state.
#' @param house Specify here whether you want boundaries for the upper or lower house.  Defaults to "upper".
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) cartographic boundary file.  Defaults
#' to TRUE (the most detailed TIGER/Line file).
#' @export

state_legislative_districts <- function(state, house = "upper", detailed = TRUE) {

  if (house == "lower") {

    type <- "sldl"

  } else if (house == "lower" & state == "31") {

    type <- "sldu"

  } else {

    type <- "sldu"

  }

  if (detailed == FALSE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_",
                  state,
                  "_",
                  type,
                  "_500k.zip")

  } else {

    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/",
                  toupper(type),
                  "/tl_2014_",
                  state,
                  "_",
                  type,
                  ".zip")

  }

  ld <- load_tiger(url)

  ld

}
