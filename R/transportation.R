#' Download a roads shapefile into R
#'
#' From the Census Bureau: "The content of the all roads shapefile includes
#' primary roads, secondary roads, local neighborhood roads,
#' rural roads, city streets, vehicular trails (4WD), ramps, service drives,
#' walkways, stairways, alleys, and private roads."
#'
#' @param state The two-digit FIPS code of the state of the county you'd like
#'        to download the roads for. Can also be state name or abbreviation
#'        (case-insensitive).
#' @param county The three-digit FIPS code of the county you'd like the roads for.
#'        Can also be a county name.
#' @param year the data year (defaults to 2015).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}).
#' @family transportation functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc.pdf}
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
roads <- function(state, county, year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2015)

  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

  state <- validate_state(state)

  county <- validate_county(state, county)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (is.null(county)) stop("Invalid county", call. = FALSE)

  url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/ROADS/tl_%s_%s%s_roads.zip",
                 as.character(year), as.character(year), state, county)

  return(load_tiger(url, tigris_type="road", ...))

}

#' Download a national primary roads shapefile into R
#'
#' From the Census Bureau: "Primary roads are generally divided,
#' limited-access highways within the Federal interstate highway
#' system or under state management. These highways are distinguished by the
#' presence of interchanges
#' and are accessible by ramps and may include some toll highways."
#'
#' @param year the data year (defaults to 2015).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}).
#' @family transportation functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#'
#' rds <- primary_roads()
#'
#' plot(rds)
#'
#' }
primary_roads <- function(year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2015)

  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

  url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/PRIMARYROADS/tl_%s_us_primaryroads.zip",
                 as.character(year), as.character(year))

  return(load_tiger(url, tigris_type="primary_roads", ...))

}

#' Download a primary & secondary roads shapefile into R
#'
#' From the Census Bureau: "Primary roads are generally divided,
#' limited-access highways within the Federal interstate highway
#' system or under state management. These highways are distinguished by the presence of interchanges
#' and are accessible by ramps and may include some toll highways. Secondary roads are main arteries,
#'  usually in the U.S. highway, state
#'  highway, or county highway system. These roads have one or more lanes of
#'  traffic in each direction, may
#'  or may not be divided, and usually have at-grade intersections with many other roads and driveways.
#'
#' @param state The two-digit FIPS code of the state of the county you'd like
#'        to download the roads for. Can also be state name or abbreviation
#'        (case-insensitive).
#' @param year the data year (defaults to 2015).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}).
#' @family transportation functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(sp)
#'
#' rds <- primary_secondary_roads()
#'
#' plot(rds)
#'
#' }
primary_secondary_roads <- function(state, year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2015)

  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/PRISECROADS/tl_%s_%s_prisecroads.zip",
                as.character(year), as.character(year), state)

  return(load_tiger(url, tigris_type="prim_sec_roads", ...))

}

#' Download a national rails shapefile into R
#'
#' National dataset for US railroads, including carlines, streetcars,
#' monorails, mass transit, cog rail, incline rail, and trams.
#'
#' @param year the data year (defaults to 2015).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}).
#' @family transportation functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(sp)
#'
#' rls <- rails()
#'
#' plot(rls)
#'
#' }
rails <- function(year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2015)

  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

  url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/RAILS/tl_%s_us_rails.zip",
                 as.character(year), as.character(year))

  return(load_tiger(url, tigris_type="rails", ...))

}
