#' Download a US Counties shapefile into R, and optionally subset by state
#'
#' @param state The two-digit FIPS code (string) of the state you want, or a
#'        vector of codes if you want multiple states. Can also be state name
#'        or state abbreviation.
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        counties file.  Defaults to TRUE (the most detailed TIGER file).
#' @export
#' @family general area functions
#' @examples \dontrun{
#' library(tigris)
#' library(ggplot2)
#' library(ggthemes)
#'
#' me <- counties("Maine", detailed=FALSE)
#' me_map <- fortify(me)
#'
#' gg <- ggplot()
#' gg <- gg + geom_map(data=me_map, map=me_map,
#'                     aes(x=long, y=lat, map_id=id),
#'                     color="black", fill="white", size=0.25)
#' gg <- gg + coord_map()
#' gg <- gg + theme_map()
#' gg
#' }
counties <- function(state = NULL, detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/COUNTY/tl_2014_us_county.zip"

  }

  ctys <- load_tiger(url)

  state <- unlist(sapply(state, validate_state, USE.NAMES=FALSE))

  if (!is.null(state)) {
    return(ctys[ctys$STATEFP %in% state,])
  } else {
    return(ctys)
  }

}

#' Download a Census tracts shapefile into R, and optionally subset by county
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param county The three-digit FIPS code (string) of the county you'd like to
#'        subset for, or a vector of FIPS codes if you desire multiple counties
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        tracts file.  Defaults to TRUE (the most detailed TIGER/Line file)
#' @family general area functions
#' @export
tracts <- function(state, county = NULL, detailed = TRUE) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

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

    return(trcts[trcts$COUNTYFP %in% county, ])

  } else {

    return(trcts)

  }

}

#' Download shapefile for all states into R
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        states file.  Defaults to TRUE (the most detailed TIGER/Line file)
#' @export
#' @family general area functions
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' states <- states(detailed=FALSE)
#'
#' leaflet(states) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5) %>%
#'   setView(-98.5795, 39.8282, zoom=3)
#' }
states <- function(detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_state_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/STATE/tl_2014_us_state.zip"

  }

  return(load_tiger(url))

}

#' Download a unified school district shapefile into R
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @family general area functions
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' schools <- school_districts("Maine")
#'
#' leaflet(schools) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5)
#' }
school_districts <- function(state) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/UNSD/tl_2014_", state, "_unsd.zip")

  return(load_tiger(url))

}

#' Download a Census-designated places shapefile into R
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to TRUE (the most detailed
#'        TIGER/Line file).
#' @family general area functions
#' @export
places <- function(state, detailed = TRUE) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (detailed == FALSE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_", state, "_place_500k.zip")

  } else {

    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/PLACE/tl_2014_", state, "_place.zip")

  }

  return(load_tiger(url))

}

#' Download a Census block groups shapefile into R, and optionally subset by county
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param county The three-digit FIPS code (string) of the county you'd like to
#'        subset for, or a vector of FIPS codes if you desire multiple counties
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        file.  Defaults to TRUE (the most detailed TIGER/Line file)
#' @family general area functions
#' @export
block_groups <- function(state, county = NULL, detailed = TRUE) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

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

    return(bgs[bgs$COUNTYFP %in% county, ])

  } else {

    return(bgs)

  }

}

#' Download a Zip Code Tabulation Area (ZCTA) shapefile into R
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        ZCTA file.  Defaults to TRUE (the most detailed TIGER/Line file).
#'        \strong{A warning:} the detailed TIGER/Line ZCTA file is massive
#'        (around 502MB unzipped), and the generalized version is also large
#'        (64MB zipped).  Be prepared for this especially if you have a slower
#'        internet connection.
#' @param starts_with Character string specifying the beginning digits of the
#'        ZCTAs you want to return.  For example, supplying the argument
#'        \code{starts_with = "761"} will return only those ZCTAs that begin
#'        with 761.  Defaults to NULL, which will return all ZCTAs in the US.
#' @family general area functions
#' @export
zctas <- function(detailed = TRUE, starts_with = NULL) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_zcta510_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/ZCTA5/tl_2014_us_zcta510.zip"

  }

  zcta <- load_tiger(url)

  if (!is.null(starts_with)) {

    return(zcta[grep(paste0("^", starts_with), zcta$ZCTA5CE10), ])

  } else {

    return(zcta)

  }

}


#' Download a Census block shapefile into R
#'
#' This function will download an entire block shapefile for a selected state
#' into R, and optionally subset by county. \strong{A warning:} Census block
#' shapefiles are often very large, especially for large states - for example, the
#' block file for Texas is 462MB zipped!  If you have a slow or unreliable internet
#' connection, or insufficient memory, this may prove burdensome given that you
#' have to first download by state and then subset.
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param county The three-digit FIPS code (string) of the county you'd like to
#'        subset for, or a vector of FIPS codes if you desire multiple counties
#' @family general area functions
#' @export
blocks <- function(state, county = NULL) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/TABBLOCK/tl_2014_",
                state,
                "_tabblock10.zip")

  blks <- load_tiger(url)

  if (!is.null(county)) {

    return(blks[blks$COUNTYFP %in% county, ])

  } else {

    return(blks)

  }

}
