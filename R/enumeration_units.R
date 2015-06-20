#' Download a US Counties shapefile into R, and optionally subset by state
#' @param state The two-digit FIPS code (string) of the state you want, or a vector of codes if you want multiple states.
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) counties file.  Defaults to TRUE
#' (the most detailed TIGER file).
#' @export

counties <- function(state = NULL, detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/COUNTY/tl_2014_us_county.zip"

  }

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
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) tracts file.  Defaults to TRUE
#' (the most detailed TIGER/Line file)
#' @export

tracts <- function(state, county = NULL, detailed = TRUE) {

  if (detailed == FALSE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_",
                  state,
                  "_tract_500k.zip")

  } else {

    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/TRACT/tl_2014_",
                  state,
                  "_tract.zip")
  }

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
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) states file.  Defaults to TRUE
#' (the most detailed TIGER/Line file)
#' @export

states <- function(detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_state_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/STATE/tl_2014_us_state.zip"

  }

  sts <- load_tiger(url)

  sts

}

#' Download a core-based statistical area shapefile into R
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) cartographic boundary file.  Defaults
#' to TRUE (the most detailed TIGER/Line file).
#' @export

cbsas <- function(detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_cbsa_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/CBSA/tl_2014_us_cbsa.zip"

  }

  cbsa <- load_tiger(url)

  cbsa

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
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) cartographic boundary file.  Defaults
#' to TRUE (the most detailed TIGER/Line file).
#' @export

places <- function(state, detailed = TRUE) {

  if (detailed == FALSE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_", state, "_place_500k.zip")

  } else {

    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/PLACE/tl_2014_", state, "_place.zip")

  }

  plcs <- load_tiger(url)

  plcs
}

#' Download a Census block groups shapefile into R, and optionally subset by county
#' @param state The two-digit FIPS code (string) of the state you want.
#' @param county The three-digit FIPS code (string) of the county you'd like to subset for, or a vector of FIPS codes
#' if you desire multiple counties
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) file.  Defaults to TRUE
#' (the most detailed TIGER/Line file)
#' @export

block_groups <- function(state, county = NULL, detailed = TRUE) {

  if (detailed == FALSE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_",
                  state,
                  "_bg_500k.zip")

  } else {

    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/BG/tl_2014_",
                  state,
                  "_bg.zip")
  }

  bgs <- load_tiger(url)

  if (!is.null(county)) {

    if (length(county) > 1) {

      sub <- bgs[bgs$COUNTYFP %in% county, ]

      sub

    } else {

      sub <- bgs[bgs$COUNTYFP == county, ]

      sub

    }

  } else {

    bgs

  }

}

#' Download a Zip Code Tabulation Area (ZCTA) shapefile into R
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k) ZCTA file.  Defaults to TRUE
#' (the most detailed TIGER/Line file)
#' @export

zctas <- function(detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_zcta510_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/ZCTA5/tl_2014_us_zcta510.zip"

  }

  zcta <- load_tiger(url)

  zcta

}


#' Download a Census block shapefile into R
#'
#' This function will download an entire block shapefile for a selected state into R, and optionally subset by county.
#' \strong{A warning:} Census block shapefiles are often very large, especially for large states - for example, the
#' block file for Texas is 462MB zipped, and XGB unzipped!  If you have a slow or unreliable internet connection,
#' or insufficient memory, this may prove burdensome given that you have to first download by state and then subset.
#'
#' @param state The two-digit FIPS code (string) of the state you want.
#' @param county The three-digit FIPS code (string) of the county you'd like to subset for, or a vector of FIPS codes
#' if you desire multiple counties
#' @export

blocks <- function(state, county = NULL) {

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/TABBLOCK/tl_2014_",
                state,
                "_tabblock10.zip")

  blks <- load_tiger(url)

  if (!is.null(county)) {

    if (length(county) > 1) {

      sub <- blks[blks$COUNTYFP %in% county, ]

      sub

    } else {

      sub <- blks[blks$COUNTYFP == county, ]

      sub

    }

  } else {

    blks

  }

}
