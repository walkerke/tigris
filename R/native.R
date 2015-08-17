#' Download an American Indian / Alaska Native / Native Hawaiian Areas shapefile into R.
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        file.  Defaults to TRUE (the most detailed TIGER/Line file)
#' @family native/tribal geometries functions
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(ggplot2)
#' library(ggthemes)
#'
#' nat <- native_areas(detailed=FALSE)
#'
#' nat_map <- fortify(nat)
#'
#' gg <- ggplot()
#' gg <- gg + geom_map(data=nat_map, map=nat_map,
#'                     aes(x=long, y=lat, map_id=id),
#'                     color="black", fill="white", size=0.25)
#' gg <- gg + coord_map(xlim=c(-179.1506, -129.9795),  # alaska
#'                      ylim=c(51.2097, 71.4410))
#' gg <- gg + theme_map()
#' gg
#' }
native_areas <- function(detailed = TRUE, ...) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_aiannh_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/AIANNH/tl_2014_us_aiannh.zip"

  }

  return(load_tiger(url, ...))

}

#' Download an American Indian Tribal Subdivision National shapefile into R.
#'
#' @family native/tribal geometries functions
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' trib <- tribal_subdivisions_national()
#' leaflet(trib) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5)
#' }
tribal_subdivisions_national <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/AITS/tl_2014_us_aitsn.zip"

  return(load_tiger(url, ...))

}

#' Download an Alaska Native Regional Corporation shapefile into R.
#'
#' @family native/tribal geometries functions
#' @export
alaska_native_regional_corporations <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/ANRC/tl_2014_02_anrc.zip"

  return(load_tiger(url, ...))

}

#' Download a Tribal block groups shapefile into R.
#'
#' @family native/tribal geometries functions
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' trib <- tribal_block_groups()
#' leaflet(trib) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5)
#' }
tribal_block_groups <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/TBG/tl_2014_us_tbg.zip"

  return(load_tiger(url, ...))

}

#' Download a Tribal Census tract shapefile into R.
#'
#' @family native/tribal geometries functions
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' trib <- tribal_census_tracts()
#' leaflet(trib) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5)
#' }
tribal_census_tracts <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/TTRACT/tl_2014_us_ttract.zip"

  return(load_tiger(url, ...))

}
