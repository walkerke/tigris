#' Download a core-based statistical area shapefile into R
#'
#' Core-based statistical areas include both metropolitan areas and micropolitan areas.  The US Census
#' Bureau defines these areas as follows: "A metro area contains a core urban area of 50,000 or more population, and a
#' micro area contains an urban core of at least 10,000 (but less than 50,000) population. Each metro or micro area
#' consists of one or more counties and includes the counties containing the core urban area, as well as any adjacent
#' counties that have a high degree of social and economic integration (as measured by commuting to work) with the urban
#' core."  Please see the link provided for more information
#'
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @param resolution The resolution of the cartographic boundary file (if cb == TRUE).
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family metro area functions
#' @seealso \url{http://www.census.gov/population/metro/}
#' @export
core_based_statistical_areas <- function(cb = FALSE, resolution = '500k', detailed = TRUE, ...) {

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
  }

  if (detailed == FALSE) {
    cb = TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
  }

  if (cb == TRUE) {
    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_cbsa_",
                  resolution,
                  ".zip")
  } else {
    url <- "http://www2.census.gov/geo/tiger/TIGER2014/CBSA/tl_2014_us_cbsa.zip"
  }

  return(load_tiger(url, tigris_type="cbsa", ...))

}

#' Download an urban areas shapefile into R
#'
#' Urban areas include both "urbanized areas," which are densely developed areas with a population of at least 50,000,
#'  and "urban clusters," which have a population of greater than 2,500 but less than 50,000.  For more information,
#' please see the link provided.
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family metro area functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2014/TGRSHP2014_TechDoc.pdf}
#' @export
urban_areas <- function(cb = FALSE, detailed = TRUE, ...) {

  if (detailed == FALSE) {
    cb = TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
  }

  if (cb == TRUE) {
    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_ua10_500k.zip"
  } else {
    url <- "http://www2.census.gov/geo/tiger/TIGER2014/UAC/tl_2014_us_uac10.zip"
  }

  return(load_tiger(url, tigris_type="urban", ...))

}

#' Download a combined statistical areas shapefile into R
#'
#' Combined statistical areas are "two or more adjacent CBSAs that have significant
#' employment interchanges."  In turn, CSAs are composed of multiple metropolitan and/or micropolitan areas, and should
#' not be compared with individual core-based statistical areas.
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @param resolution The resolution of the cartographic boundary file (if cb == TRUE).
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family metro area functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2014/TGRSHP2014_TechDoc.pdf}
#' @export
combined_statistical_areas <- function(cb = FALSE, resolution = '500k', detailed = TRUE, ...) {

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
  }

  if (detailed == FALSE) {
    cb = TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
  }

  if (cb == TRUE) {
    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_csa_",
                  resolution,
                  ".zip")
  } else {
    url <- "http://www2.census.gov/geo/tiger/TIGER2014/CSA/tl_2014_us_csa.zip"
  }

  return(load_tiger(url, tigris_type="csa", ...))

}

#' Download a metropolitan divisions shapefile into R.
#'
#' Metropolitan divisions are subdivisions of metropolitan areas with population of at least 2.5 million.  Please note:
#' not all metropolitan areas have metropolitan divisions.
#'
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family metro area functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2014/TGRSHP2014_TechDoc.pdf}
#' @export
metro_divisions <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/METDIV/tl_2014_us_metdiv.zip"

  return(load_tiger(url, tigris_type="metro", ...))

}

#' Download a New England City and Town Area shapefile into R
#'
#' From the US Census Bureau (see link for source): "In New England
#' (Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island,
#' and Vermont), the OMB has defined an alternative county subdivision
#' (generally city and town) based definition of CBSAs
#' known as New England city and town areas (NECTAs). NECTAs are defined using the same criteria as
#' metropolitan and micropolitan statistical areas and are identified as either metropolitan or micropolitan,
#' based, respectively, on the presence of either an urbanized area of 50,000 or more inhabitants or an
#' urban cluster of at least 10,000 and less than 50,000 inhabitants."  Combined NECTAs, or CNECTAs, are two or more
#' NECTAs that have significant employment interchange, like Combined Statistical Areas;
#' NECTA divisions are subdivisions of NECTAs.
#'
#' @param type Specify whether to download the New England City and Town Areas file (\code{'necta'}, the default),
#'        the combined NECTA file (\code{'combined'}), or the NECTA divisions file (\code{'divisions'}).
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).  Only available when \code{type = 'necta'}.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family metro area functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2014/TGRSHP2014_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(sp)
#'
#' ne <- new_england(cb = TRUE)
#'
#' plot(ne)
#'
#' }
new_england <- function(type = 'necta', cb = FALSE, ...) {

  if (type == 'necta') {

    if (cb == TRUE) {

      url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_necta_500k.zip"

    } else {

      url <- "http://www2.census.gov/geo/tiger/TIGER2014/NECTA/tl_2014_us_necta.zip"

    }

    return(load_tiger(url, tigris_type = "necta", ...))

  } else if (type == 'combined') {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/CNECTA/tl_2014_us_cnecta.zip"

    return(load_tiger(url, tigris_type = "cnecta", ...))

  } else if (type == 'divisions') {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/NECTADIV/tl_2014_us_nectadiv.zip"

    return(load_tiger(url, tigris_type = "nectadiv", ...))

  } else {

    stop("Invalid NECTA type; valid values include 'necta' (the default), 'combined', and 'divisions'.", call. = FALSE)

  }


}
