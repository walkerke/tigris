#' Download a US regions cartographic boundary shapefile into R
#'
#' @param resolution The resolution of the cartographic boundary file.
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
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

  year <- set_tigris_year(year)

  check_tigris_resolution(resolution)

  url <- url_tiger("GENZ%s/shp/cb_%s_us_region_%s",
                 year, year, resolution)

  rgns <- load_tiger(url, tigris_type = "region", ...)

  return(rgns)

}

#' Download a US Census divisions cartographic boundary shapefile into R
#'
#' @param resolution The resolution of the cartographic boundary file.
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
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

  year <- set_tigris_year(year)

  check_tigris_resolution(resolution)

  url <- url_tiger("GENZ%s/shp/cb_%s_us_division_%s",
                 year, year, resolution)

  div <- load_tiger(url, tigris_type = "division", ...)

  return(div)

}

#' Download a US national boundary shapefile into R
#'
#' @param resolution The resolution of the cartographic boundary file.
#'        Defaults to '5m'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
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

  year <- set_tigris_year(year)

  check_tigris_resolution(resolution, values = c('5m', '20m'))

  url <- url_tiger("GENZ%s/shp/cb_%s_us_nation_%s",
                 year, year, resolution)

  nat <- load_tiger(url, tigris_type = "nation", ...)

  return(nat)

}

