#' Download an area water shapefile into R
#'
#' From the US Census Bureau: "The area hydrography shapefile contains the geometry
#' and attributes of both perennial and intermittent
#' area hydrography features, including ponds, lakes, oceans, swamps, glaciers, and the area covered by
#' large streams represented as double-line drainage."
#'
#' @param state The two-digit FIPS code of the state of the county you'd like to
#'        download the water features for.  Can also be state name or abbreviation
#'        (case-insensitive).
#' @param county The three-digit FIPS code of the county you'd like the water
#'        features for.  Can also be a county name.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family water functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2014/TGRSHP2014_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(sp)
#'
#' dallas_water <- area_water("TX", "Dallas")
#'
#' plot(dallas_water)
#'
#' }
area_water <- function(state, county, ...) {

  state <- validate_state(state)

  county <- validate_county(state, county)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (is.null(county) | length(county) > 1) stop("Invalid county", call. = FALSE)

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/AREAWATER/tl_2014_",
                state,
                county,
                "_areawater.zip")

  return(load_tiger(url, tigris_type="area_water", ...))

}

#' Download an linear water shapefile into R
#'
#' From the US Census Bureau: "The linear hydrography shapefile contains all linear
#' features with "H" (Hydrography) type MTFCCs in the
#' MAF/TIGER database by county. The shapefiles are provided at a county geographic extent and in linear
#' elemental feature geometry. The linear hydrography shapefile includes streams/rivers, braided streams,
#' canals, ditches, artificial paths, and aqueducts. A linear hydrography feature may include edges with both
#' perennial and intermittent persistence."
#'
#' @param state The two-digit FIPS code of the state of the county you'd like to
#'        download the water features for. Can also be state name or abbreviation
#'        (case-insensitive).
#' @param county The three-digit FIPS code of the county you'd like the water
#'        features for.  Can also be a county name.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family water functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2014/TGRSHP2014_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(sp)
#'
#' dallas_water <- linear_water("TX", "Dallas")
#'
#' plot(dallas_water)
#'
#' }
linear_water <- function(state, county, ...) {

  state <- validate_state(state)

  county <- validate_county(state, county)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (is.null(county)) stop("Invalid county", call. = FALSE)

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/LINEARWATER/tl_2014_",
                state,
                county,
                "_linearwater.zip")

  return(load_tiger(url, tigris_type="linear_water", ...))

}

#' Download a shapefile of the US coastline into R
#'
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @export
#' @family water functions
#' @examples \dontrun{
#' library(tigris)
#' library(ggplot2)
#' library(ggthemes)
#' library(rgeos)
#' library(sp)
#'
#' coast <- coastline()
#'
#' # ggplot really doesn't like a ton of detailed lines so
#' # we use rgeos::gSimplify to get the structure to a reasonable size
#' # but we also lose the SpatialLinesDataFrame, so re-bind the
#' # data from the original spatial structure so we can use fortify
#'
#' coast_simp <- gSimplify(coast, tol=1/200, topologyPreserve=TRUE)
#' coast_simp <- SpatialLinesDataFrame(coast_simp, coast@@data)
#'
#' coast_map <- fortify(coast_simp)
#'
#' gg <- ggplot()
#' gg <- gg + geom_map(data=coast_map, map=coast_map,
#'                     aes(x=long, y=lat, map_id=id),
#'                     color="black", fill="white", size=0.25)
#' gg <- gg + coord_map(xlim=c(-125.0011, -66.9326),
#'                      ylim=c(24.9493, 49.5904))
#' gg <- gg + theme_map()
#' gg
#' }
coastline <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/COAST/tl_2014_us_coastline.zip"

  return(load_tiger(url, tigris_type="coastline", ...))

}
