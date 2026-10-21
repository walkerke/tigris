#' Download and use U.S. Census TIGER shapefiles in R
#'
#' tigris is an R package that allows users to directly download and use TIGER/Line and
#' cartographic boundary shapefiles from the US Census Bureau in R. For an overview of the package, visit its repository at \url{https://github.com/walkerke/tigris} or read Chapter 5 of the book \emph{Analyzing US Census Data: Methods, Maps, and Models in R} at \url{https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html}.
#'
#' Use option \code{tigris_use_cache} to tell \code{tigris} to cache
#' Census shapefile downloads. This is \code{FALSE} by default. e.g.
#' \code{options(tigris_use_cache=TRUE)}
#'
#' Use option \code{tigris_refresh} to force a refresh of cached \code{tigris}
#' Shapefiles. e.g. \code{options(tigris_refresh=TRUE)}
#'
#' Use option \code{tigris_year} to change the year for which you'd like to download data.
#' e.g. \code{options(tigris_year = 2017)}.  The default year for the package is 2025.
#'
#' Use option \code{tigris_class} to specify the class of spatial object you'd like returned.
#' The default is \code{"sf"} for simple features objects.  If you'd like a legacy object
#' of class \code{Spatial*DataFrame}, use \code{options(tigris_class = "sp")}. Please note
#' that legacy sp objects are no longer formally supported in tigris.
#'
#' Use option \code{tigris_crs} to specify the coordinate reference system you'd like returned.
#' The default is \code{"EPSG:4269"} which is the existing coordinate reference system for the TIGER/Line Shapefiles.
#'
#' Use the \code{protocol} argument in data download functions to specify whether to use FTP or HTTP
#' for downloading files. The default is \code{"http"}, which downloads over HTTPS and falls back to FTP
#' if the download fails. Use \code{protocol = "ftp"}, which may work better in some environments
#' where HTTPS connections are restricted, to download over FTP only.
#'
#' Downloads have no time limit, and are retried automatically if the Census Bureau's servers are
#' temporarily unavailable. The \code{timeout} parameter is no longer used.
#'
#' @note Several \code{options} and arguments control behavior of various \code{tigris} functions.
#'       See \code{Details} for more information.
#' @name tigris-package
#' @aliases tigris
#' @author Kyle Walker (@@kyle_e_walker)
#' @importFrom stringr str_trim str_pad
#' @import utils
#' @import rappdirs
#' @import sf
#' @import dplyr
#' @importFrom cli cli_abort cli_warn cli_bullets
#' @import rlang
#' @importFrom methods as
NULL

#' @keywords internal
"_PACKAGE"

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
