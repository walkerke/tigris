#' Download a congressional districts shapefile for the 114th Congress into R
#'
#' Description from the US Census Bureau (see link for source):
#' The 2015 TIGER/Line Shapefiles contain the 114th Congressional Districts. All congressional districts
#' appearing in the 2015 TIGER/Line Shapefiles reflect the information provided to the Census Bureau by
#' the states by May 1, 2014. The 114th Congressional District shapefile contains the areas in effect
#' January 2015 to 2017.
#'
#' Congressional districts are the 435 areas from which people are elected to the U.S. House of
#' Representatives and the five areas with nonvoting delegates from state equivalents. After the
#' apportionment of congressional seats among the states based on decennial census population counts,
#' each state is responsible for establishing the boundaries of the congressional districts for the purpose of
#' electing representatives. Each congressional district is to be as equal in population to all other
#' congressional districts in a state as practicable.
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @param resolution The resolution of the cartographic boundary file (if cb == TRUE).
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2015).
#' @family legislative district functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' cd114 <- congressional_districts(cb = TRUE, resolution = '20m')
#'
#' leaflet(cd114) %>%
#'    addTiles() %>%
#'    addPolygons()
#' }
congressional_districts <- function(cb = FALSE, resolution = '500k', detailed = TRUE, ...) {

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
  }

  if (detailed == FALSE) {
    cb = TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
  }

  if (cb == TRUE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_cd114_",
                  resolution,
                  ".zip")
  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2015/CD/tl_2015_us_cd114.zip"

  }

  return(load_tiger(url, tigris_type="congressional_districts", ...))

}

#' Download a state legislative districts shapefile into R - upper or lower
#'
#' This function allows you to download boundaries for state legislatures into R.
#' Generally, state legislatures are comprised of an "upper" house, which is
#' typically referred to as the Senate, and a "lower" house, which is often (but
#' not exclusively) referred to as the House.  The exception is Nebraska, which
#' has a unicameral state legislature.
#'
#' @param state The two-digit FIPS code (string) of the state. Can also be state
#'        name or abbreviation (case-insensitive)
#' @param house Specify here whether you want boundaries for the \code{upper} or
#'        \code{lower} house.  Defaults to \code{upper}.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2015).
#' @family legislative district functions
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' leg <- state_legislative_districts("Maine", "lower", cb = TRUE)
#'
#' leaflet(leg) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5)
#' }
state_legislative_districts <- function(state, house = "upper", cb = FALSE, detailed = TRUE, ...) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (!house %in% c("upper", "lower"))
    stop("Must specify 'upper' or 'lower' for 'house' parameter", call.=FALSE)

  if (house == "lower" & state == "31") { # Nebraska

    type <- "sldu"

  } else if (house == "lower") {

    type <- "sldl"

  } else {

    type <- "sldu"

  }

  if (detailed == FALSE) {
    cb = TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
  }

  if (cb == TRUE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_",
                  state,
                  "_",
                  type,
                  "_500k.zip")

  } else {

    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2015/",
                  toupper(type),
                  "/tl_2015_",
                  state,
                  "_",
                  type,
                  ".zip")

  }

  return(load_tiger(url, tigris_type="state_legislative_districts", ...))

}

#' Download a voting districts shapefile (2012 TIGER/Line) into R
#'
#' This function allows you to download a voting districts boundary file into R.  The voting districts
#' shapefile is found in the 2012 TIGER/Line dataset, and has not been updated since then. The Census Bureau (see link
#' for source) describes voting districts as follows: " 'Voting district' is the generic name for geographic
#' entities such as precincts, wards, and election districts established by state and local governments for
#' the purpose of conducting elections. States participating in the Census 2010 Redistricting Data Programs
#' as part of Public Law 94-171 (1975) provided the Census Bureau with boundaries, codes, and names
#' for their voting districts."
#'
#' @param state The state for which you'd like to retrieve data.  Can be a state name,
#'        state abbreviation, or FIPS code.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}).
#'
#' @family legislative district functions
#' @export
#' @examples \dontrun{#'
#' library(tigris)
#' library(sp)
#'
#' ia <- voting_districts("Iowa")
#'
#' plot(ia)
#'
#' }
voting_districts <- function(state) {

  message("The voting districts shapefiles are from the 2012 TIGER/Line dataset.")

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2012/VTD/tl_2012_",
                state,
                "_vtd10.zip")

  return(load_tiger(url, tigris_type = 'voting_districts'))

}
