#' Download a core-based statistical area shapefile into R
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

#' Download an urbanized areas shapefile into R
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
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family metro area functions
#' @export
metro_divisions <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/METDIV/tl_2014_us_metdiv.zip"

  return(load_tiger(url, tigris_type="metro", ...))

}
