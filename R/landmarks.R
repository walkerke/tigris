#' Download the Military Installation National Shapefile into R
#'
#' Description from the US Census Bureau: "The Census Bureau includes landmarks
#' such as military installations in the MAF/TIGER database for
#' locating special features and to help enumerators during field operations. The Census Bureau adds
#' landmark features to the database on an as-needed basis and does not attempt to ensure that all
#' instances of a particular feature are included. For additional information about area landmarks, please
#' see Section 3.12, Landmarks (Area and Point)."
#'
#' This file does not include the three point landmarks identified as military installation features in the
#' MAF/TIGER database. These point landmarks are included in the point landmark shapefile.
#' Although almost all military installations have assigned 8-character National Standard (GNIS) codes, the
#' Census Bureau has not loaded most of this data into the MAF/TIGER database. The 2015 military
#' shapefiles contain few values in the ANSICODE field.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2015).
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc_Ch3.pdf}
#' @export

military <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2015/MIL/tl_2015_us_mil.zip"

  return(load_tiger(url, tigris_type = "military", ...))

}


#' Download a point or area landmarks shapefile into R
#'
#' Description from the US Census Bureau:
#' "The Census Bureau includes landmarks in the MAF/TIGER database (MTDB) for locating special features
#' and to help enumerators during field operations. Some of the more common landmark types include area
#' landmarks such as airports, cemeteries, parks, and educational facilities and point landmarks such as
#' schools and churches."
#'
#' The Census Bureau adds landmark features to the database on an as-needed basis and makes no
#' attempt to ensure that all instances of a particular feature were included. The absence of a landmark
#' such as a hospital or prison does not mean that the living quarters associated with that landmark were
#' excluded from the 2010 Census enumeration. The landmarks were not used as the basis for building or
#' maintaining the address list used to conduct the 2010 Census.
#'
#' Area landmark and area water features can overlap; for example, a park or other special land-use feature
#' may include a lake or pond. In this case, the polygon covered by the lake or pond belongs to a water
#' feature and a park landmark feature. Other kinds of landmarks can overlap as well. Area landmarks can
#' contain point landmarks, but these features are not linked in the TIGER/Line Shapefiles.
#'
#' Landmarks may be identified by a MAF/TIGER feature class code only and may not have a name. Each
#' landmark has a unique area landmark identifier (AREAID) or point landmark identifier (POINTID) value.
#'
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc_Ch3.pdf}
#'
#' @param state The state for which you'd like to download the landmarks
#' @param type Whether you would like to download point landmarks (\code{"point"}) or area landmarks (\code{"area"}).                   Defaults to \code{"point"}.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2015).
#' @export
landmarks <- function(state, type = "point", ...) {

  state <- validate_state(state)

  if (type == "area") {
    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2015/AREALM/tl_2015_", state, "_arealm.zip")
    return(load_tiger(url, tigris_type = "area_landmark", ...))

  } else if (type == "point") {
    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2015/POINTLM/tl_2015_", state, "_pointlm.zip")
    return(load_tiger(url, tigris_type = "point_landmark", ...))

  } else {
    stop('The argument supplied to type must be either "point" or "area"', call. = FALSE)
  }

}
