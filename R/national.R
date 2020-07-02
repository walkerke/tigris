#' Download a US regions cartographic boundary shapefile into R
#'
#' @param resolution The resolution of the cartographic boundary file.
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @param year the data year (defaults to 2018).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{class}, which can be set to \code{"sf"} (the default) or \code{"sp"} to
#'        request sf or sp class objects, and \code{refresh}, which specifies whether or
#'        not to re-download shapefiles (defaults to \code{FALSE}).
#'
#' @family national cartographic boundary functions
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' us_regions <- regions(resolution = '20m')
#'
#' leaflet(us_regions) %>%
#'    addTiles() %>%
#'    addPolygons()
#' }
#' @export
regions <- function(resolution = '500k', year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2018)

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

  url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_region_%s.zip",
                 cyear, cyear, resolution)

  rgns <- load_tiger(url, tigris_type = "region", ...)

  return(rgns)

}

#' Download a US Census divisions cartographic boundary shapefile into R
#'
#' @param resolution The resolution of the cartographic boundary file.
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @param year the data year (defaults to 2018).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{class}, which can be set to \code{"sf"} (the default) or \code{"sp"} to
#'        request sf or sp class objects, and \code{refresh}, which specifies whether or
#'        not to re-download shapefiles (defaults to \code{FALSE}).
#'
#' @family national cartographic boundary functions
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' divs <- divisions(resolution = '20m')
#'
#' leaflet(divs) %>%
#'    addTiles() %>%
#'    addPolygons()
#' }
divisions <- function(resolution = '500k', year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2018)

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

  url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_division_%s.zip",
                 cyear, cyear, resolution)

  div <- load_tiger(url, tigris_type = "division", ...)

  return(div)

}

#' Download a US national boundary shapefile into R
#'
#' @param resolution The resolution of the cartographic boundary file.
#'        Defaults to '5m'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @param year the data year (defaults to 2018).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{class}, which can be set to \code{"sf"} (the default) or \code{"sp"} to
#'        request sf or sp class objects, and \code{refresh}, which specifies whether or
#'        not to re-download shapefiles (defaults to \code{FALSE}).
#'
#' @family national cartographic boundary functions
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' boundary <- nation(resolution = '20m')
#'
#' leaflet(boundary) %>%
#'    addTiles() %>%
#'    addPolygons()
#' }
nation <- function(resolution = '5m', year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2018)

  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

  if (!(resolution %in% c('5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '5m', and '20m'.", call. = FALSE)
  }

  cyear <- as.character(year)

  url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_nation_%s.zip",
                 cyear, cyear, resolution)

  nat <- load_tiger(url, tigris_type = "nation", ...)

  return(nat)

}

