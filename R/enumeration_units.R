#' Download a US Counties shapefile into R, and optionally subset by state
#' @param state The two-digit FIPS code (string) of the state you want, or a vector of codes if you want multiple states.
#' @export

counties <- function(state = NULL) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/COUNTY/tl_2014_us_county.zip"

  ctys <- load_tiger(url)

  if (!is.null(state)) {

    if (length(state) > 1) {

      sub <- ctys[ctys$STATEFP %in% state, ]

      sub

    } else {

      sub <- ctys[ctys$STATEFP == state, ]

      sub

    }

  } else {

    ctys

  }

}

#' Download a Census tracts shapefile into R, and optionally subset by county
#' @param state The two-digit FIPS code (string) of the state you want.
#' @param county The three-digit FIPS code (string) of the county you'd like to subset for, or a vector of FIPS codes
#' if you desire multiple counties
#' @export

tracts <- function(state, county = NULL) {

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/TRACT/tl_2014_",
                state,
                "_tract.zip")

  trcts <- load_tiger(url)

  if (!is.null(county)) {

    if (length(county) > 1) {

      sub <- trcts[trcts$COUNTYFP %in% county, ]

      sub

    } else {

      sub <- trcts[trcts$COUNTYFP == county, ]

      sub

    }

  } else {

    trcts

  }

}

#' Download a states shapefile into R
#'
#' @export

states <- function() {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/STATE/tl_2014_us_state.zip"

  sts <- load_tiger(url)

  sts

}

#' Download a core-based statistical area shapefile into R
#'
#' @export

cbsas <- function() {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/CBSA/tl_2014_us_cbsa.zip"

  cbsa <- load_tiger(url)

  cbsa

}

#' Download a congressional districts shapefile for the 114th Congress into R
#'
#' @export

congressional_districts <- function() {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/CD/tl_2014_us_cd114.zip"

  cd <- load_tiger(url)

  cd

}

#' Download a unified school district shapefile into R
#'
#' @param state The two-digit FIPS code (string) of the state you want.
#' @export

school_districts <- function(state) {

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/UNSD/tl_2014_", state, "_unsd.zip")

  sd <- load_tiger(url)

  sd

}

#' Download a Census-designated places shapefile into R
#'
#' @param state The two-digit FIPS code (string) of the state you'd like to download.
#' @export

places <- function(state) {

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/PLACE/tl_2014_", state, "_place.zip")

  plcs <- load_tiger(url)

  plcs
}
