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
#' @param year the data year (defaults to 2019).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{class}, which can be set to \code{"sf"} (the default) or \code{"sp"} to
#'        request sf or sp class objects, and \code{refresh}, which specifies whether or
#'        not to re-download shapefiles (defaults to \code{FALSE}).
#' @family water functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2019/TGRSHP2019_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#'
#' dallas_water <- area_water("TX", "Dallas")
#'
#' plot(dallas_water$geometry)
#'
#' }
area_water <- function(state, county, year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2019)

  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

  if (length(county) > 1) {
    w <- lapply(county, function(x) {
      area_water(state = state, county = x, year = year, ...)
    }) %>%
      rbind_tigris()

    return(w)
  }

  state <- validate_state(state)

  county <- validate_county(state, county)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (is.null(county) | length(county) > 1) stop("Invalid county", call. = FALSE)

  cyear <- as.character(year)

  url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/AREAWATER/tl_%s_%s%s_areawater.zip",
                 cyear, cyear, state, county)

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
#' @param year the data year (defaults to 2019).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{class}, which can be set to \code{"sf"} (the default) or \code{"sp"} to
#'        request sf or sp class objects, and \code{refresh}, which specifies whether or
#'        not to re-download shapefiles (defaults to \code{FALSE}).
#' @family water functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2019/TGRSHP2019_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#'
#' dallas_water <- linear_water("TX", "Dallas")
#'
#' plot(dallas_water$geometry)
#'
#' }
linear_water <- function(state, county, year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2019)

  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

  if (length(county) > 1) {
    w <- lapply(county, function(x) {
      linear_water(state = state, county = x, year = year, ...)
    }) %>%
      rbind_tigris()

    return(w)
  }

  state <- validate_state(state)

  county <- validate_county(state, county)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (is.null(county)) stop("Invalid county", call. = FALSE)

  cyear <- as.character(year)

  url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/LINEARWATER/tl_%s_%s%s_linearwater.zip",
                 cyear, cyear, state, county)

  return(load_tiger(url, tigris_type="linear_water", ...))

}

#' Download a shapefile of the US coastline into R
#'
#' @param year The year of the dataset (defaults to 2019)
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{class}, which can be set to \code{"sp"} (the default) or \code{"sf"} to
#'        request sp or sf class objects, and \code{refresh}, which specifies whether or
#'        not to re-download shapefiles (defaults to \code{FALSE}).).
#' @export
#' @family water functions
coastline <- function(year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2019)

  }

  cyear <- as.character(year)

  if (year > 2016) {
    url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/COASTLINE/tl_%s_us_coastline.zip",
                   cyear, cyear)
  } else {
    url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/COAST/tl_%s_us_coastline.zip",
                   cyear, cyear)
  }



  return(load_tiger(url, tigris_type="coastline", ...))

}
