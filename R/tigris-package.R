#' Download and use U.S. Census TIGER shapefiles in R
#'
#' tigris is an R package that allows users to directly download and use TIGER/Line and
#' cartographic boundary shapefiles from the US Census Bureau in R. For an overview of the package, visit its repository at <https://github.com/walkerke/tigris> or read Chapter 5 of the book *Analyzing US Census Data: Methods, Maps, and Models in R* at <https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html>.
#'
#' Use option `tigris_use_cache` to tell `tigris` to cache
#' Census shapefile downloads. This is `FALSE` by default. e.g.
#' `options(tigris_use_cache=TRUE)`
#'
#' Use option `tigris_refresh` to force a refresh of cached `tigris`
#' Shapefiles. e.g. `options(tigris_refresh=TRUE)`
#'
#' Use option `tigris_year` to change the year for which you'd like to download data.
#' e.g. `options(tigris_year = 2017)`.  The default year for the package is 2020.
#'
#' Use option `tigris_class` to specify the class of spatial object you'd like returned.
#' The default is `"sf"` for simple features objects.  If you'd like a legacy object
#' of class `Spatial*DataFrame`, use `options(tigris_class = "sp")`. Please note
#' that legacy sp objects are no longer formally supported in tigris.
#' @note Four `options` control behavior of various `tigris` functions.
#'       See `Details` for more information.
#' @author Kyle Walker (@@kyle_e_walker)
#' @aliases tigris
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import utils
#' @import httr
#' @import sf
#' @import dplyr
#' @import rlang
#' @importFrom methods as
## usethis namespace: end
NULL

#' tigris exported operators
#'
#'
#' @name tigris-exports
NULL

#' Pipe operator
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @export
#' @rdname tigris-exports
NULL


