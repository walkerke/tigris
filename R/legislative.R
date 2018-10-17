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
#' @param year the data year (defaults to 2016).  To get boundaries for the 115th congress, set \code{year = 2016}.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}).
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
congressional_districts <- function(cb = FALSE, resolution = '500k', year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2016)

  }

  if (year == 2018) {
    congress <- "116"
  } else if (year == 2016) {
    congress <- "115"
  } else if (year %in% 2014:2015) {
    congress <- "114"
  } else if (year == 2013) {
    congress <- "113"
  } else if (year %in% 2011:2012) {
    congress <- "112"
  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
  }

  cyear <- as.character(year)

  if (cb == TRUE) {

    url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_cd%s_%s.zip",
                   cyear, cyear, congress, resolution)

    if (year == 2013) url <- gsub("shp/", "", url)

  } else {

    url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/CD/tl_%s_us_cd%s.zip",
                   cyear, cyear, congress)

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
#' @param year the data year (defaults to 2016).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}).
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
state_legislative_districts <- function(state, house = "upper", cb = FALSE, year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2016)

  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

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

  cyear <- as.character(year)


  if (cb == TRUE) {

    if (year == 2010) {
      if (type == "sldu") {
        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_610_u2_500k.zip",
                       state)
      } else if (type == "sldl") {
        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_620_l2_500k.zip",
                       state)
      }
    }

    url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_%s_500k.zip",
                   cyear, cyear, state, type)

    if (year == 2013) url <- gsub("shp/", "", url)

  } else {

    if (year %in% c(2000, 2010)) {
      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/%s/%s/tl_2010_%s_%s%s.zip",
                     toupper(type), cyear, state, type, substr(cyear, 3, 4))
    } else {
      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/%s/tl_%s_%s_%s.zip",
                     cyear, toupper(type), cyear, state, type)
    }

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
voting_districts <- function(state, ...) {

  message("The voting districts shapefiles are from the 2012 TIGER/Line dataset.")

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  url <- paste0("https://www2.census.gov/geo/tiger/TIGER2012/VTD/tl_2012_",
                state,
                "_vtd10.zip")

  return(load_tiger(url, tigris_type = 'voting_districts', ...))

}


