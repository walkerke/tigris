#' Download a roads shapefile into R
#'
#' @param state The two-digit FIPS code of the state of the county you'd like
#'        to download the roads for. Can also be state name or abbreviation
#'        (case-insensitive).
#' @param county The three-digit FIPS code of the county you'd like the roads for
#' @family transportation functions
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(ggplot2)
#' library(ggthemes)
#' library(rgeos)
#' library(sp)
#'
#' roads <- roads("Maine", "031")
#'
#' # for ggplot, we need to simplify the lines otherwise it'll take
#' # forever to plot. however, gSimplify whacks the SpatialLinesDataFrame
#' # so we need to re-bind the data from the original object to it so
#' # we can use "fortify"
#'
#' roads_simp <- gSimplify(roads, tol=1/200, topologyPreserve=TRUE)
#' roads_simp <- SpatialLinesDataFrame(roads_simp, roads@@data)
#'
#' roads_map <- fortify(roads_simp) # this takes a bit
#'
#' gg <- ggplot()
#' gg <- gg + geom_map(data=roads_map, map=roads_map,
#'                     aes(x=long, y=lat, map_id=id),
#'                     color="black", fill="white", size=0.25)
#' gg <- gg + coord_map()
#' gg <- gg + theme_map()
#' gg
#' }
roads <- function(state, county, ...) {

  state <- validate_state(state)

  county <- validate_county(state, county)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (is.null(county)) stop("Invalid county", call. = FALSE)

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/ROADS/tl_2014_",
                state,
                county,
                "_roads.zip")

  return(load_tiger(url, tigris_type="road", ...))

}

#' Download a national primary roads shapefile into R
#'
#' @family transportation functions
#' @export
primary_roads <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/PRIMARYROADS/tl_2014_us_primaryroads.zip"

  return(load_tiger(url, tigris_type="primary_roads", ...))

}

#' Download a primary & secondary roads shapefile into R
#'
#' @param state The two-digit FIPS code of the state of the county you'd like
#'        to download the roads for. Can also be state name or abbreviation
#'        (case-insensitive).
#' @family transportation functions
#' @export
primary_secondary_roads <- function(state, ...) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/PRISECROADS/tl_2014_",
                state,
                "_prisecroads.zip")

  return(load_tiger(url, tigris_type="prim_sec_roads", ...))

}

#' Download a national rails shapefile into R
#'
#' @family transportation functions
#' @export
rails <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/RAILS/tl_2014_us_rails.zip"

  return(load_tiger(url, tigris_type="rails", ...))

}
