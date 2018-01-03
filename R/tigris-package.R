#' Download and use U.S. Census TIGER shapefiles in R
#'
#' Use option \code{tigris_use_cache} to tell \code{tigris} to cache
#' Census shapefile downloads. This is \code{TRUE} by default. e.g.
#' \code{options(tigris_use_cache=TRUE)}
#'
#' Use option \code{tigris_refresh} to force a refresh of cached \code{tigris}
#' Shapefiles. e.g. \code{options(tigris_refresh=TRUE)}
#'
#' Use option \code{tigris_year} to change the year for which you'd like to download data.
#' e.g. \code{options(tigris_year = 2013)}.  The default year for the package is 2015, the most
#' recent year for which data are available.
#'
#' @note Four \code{options} control behavior of various \code{tigris} functions.
#'       See \code{Description} for more information.
#' @author Kyle Walker
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
#' The following functions are imported and then re-exported
#' from the tigris package to enable use of the magrittr
#' pipe operator and the sp plot method without any additional
#' library calls
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

#' Spatial plotting
#'
#' @importFrom sp plot
#' @name plot
#' @export
#' @rdname tigris-exports
NULL
