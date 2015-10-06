#' Download a congressional districts shapefile for the 114th Congress into R
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @param resolution The resolution of the cartographic boundary file (if cb == TRUE).
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @family legislative district functions
#' @export
congressional_districts <- function(cb = FALSE, resolution = '500k', detailed = TRUE, ...) {

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
  }

  if (detailed == FALSE) {
    cb = TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
  }

  if (cb == TRUE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_cd114_",
                  resolution,
                  ".zip")
  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/CD/tl_2014_us_cd114.zip"

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

  return(load_tiger(url, tigris_type="state_legislative_districts", ...))

}
