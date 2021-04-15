#' Download and use U.S. Census TIGER shapefiles in R
#'
#' tigris is an R package that allows users to directly download and use TIGER/Line and
#' cartographic boundary shapefiles from the US Census Bureau in R.  As of version 1.0
#' (released in July 2020) tigris functions return simple features (sf) objects with a default
#' year of 2019.  For an overview of the package, visit its repository at \url{https://github.com/walkerke/tigris}.
#'
#' Use option \code{tigris_use_cache} to tell \code{tigris} to cache
#' Census shapefile downloads. This is \code{FALSE} by default. e.g.
#' \code{options(tigris_use_cache=TRUE)}
#'
#' Use option \code{tigris_refresh} to force a refresh of cached \code{tigris}
#' Shapefiles. e.g. \code{options(tigris_refresh=TRUE)}
#'
#' Use option \code{tigris_year} to change the year for which you'd like to download data.
#' e.g. \code{options(tigris_year = 2017)}.  The default year for the package is 2019, the most
#' recent year for which data are available.
#'
#' Use option \code{tigris_class} to specify the class of spatial object you'd like returned.
#' The default is \code{"sf"} for simple features objects.  If you'd like a legacy object
#' of class \code{Spatial*DataFrame}, use \code{options(tigris_class = "sp")}.
#'
#' @note Four \code{options} control behavior of various \code{tigris} functions.
#'       See \code{Details} for more information.
#' @name tigris
#' @docType package
#' @author Kyle Walker (@@kyle_e_walker)
#' @importFrom stringr str_trim str_pad
#' @import maptools
#' @import rgdal
#' @import sp
#' @import utils
#' @import rappdirs
#' @import httr
#' @import uuid
#' @import sf
#' @import dplyr
#' @importFrom methods as
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


