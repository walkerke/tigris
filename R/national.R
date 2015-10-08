#' Download a US regions cartographic boundary shapefile into R
#'
#' @param resolution The resolution of the cartographic boundary file.
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
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
regions <- function(resolution = '500k') {

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
  }

  url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_region_",
                resolution,
                ".zip")

  rgns <- load_tiger(url, tigris_type = "region")

  return(rgns)

}

#' Download a US Census divisions cartographic boundary shapefile into R
#'
#' @param resolution The resolution of the cartographic boundary file.
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
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
divisions <- function(resolution = '500k') {

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
  }

  url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_division_",
                resolution,
                ".zip")

  div <- load_tiger(url, tigris_type = "division")

  return(div)

}

#' Download a US national boundary shapefile into R
#'
#' @param resolution The resolution of the cartographic boundary file.
#'        Defaults to '5m'; options include '5m' (1:5 million) and '20m' (1:20 million).
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
nation <- function(resolution = '5m') {

  if (!(resolution %in% c('5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '5m', and '20m'.", call. = FALSE)
  }

  url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_nation_",
                resolution,
                ".zip")

  nat <- load_tiger(url, tigris_type = "nation")

  return(nat)

}

