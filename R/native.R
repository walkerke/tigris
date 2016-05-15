#' Download an American Indian / Alaska Native / Native Hawaiian Areas shapefile into R.
#'
#' Description from the Census Bureau: "This shapefile contain both legal and statistical
#' American Indian, Alaska Native, and Native Hawaiian
#' entities for which the Census Bureau publishes data. The legal entities consist of federally recognized
#' American Indian reservations and off-reservation trust land areas, state-recognized American Indian
#' reservations, and Hawaiian home lands (HHLs)." For more information, please see the link provided.
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        file.  Defaults to FALSE (the most detailed TIGER/Line file)
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2015).
#' @family native/tribal geometries functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(ggplot2)
#' library(ggthemes)
#'
#' nat <- native_areas(cb = TRUE)
#'
#' nat_map <- fortify(nat)
#'
#' gg <- ggplot()
#' gg <- gg + geom_map(data=nat_map, map=nat_map,
#'                     aes(x=long, y=lat, map_id=id),
#'                     color="black", fill="white", size=0.25)
#' gg <- gg + coord_map(xlim=c(-179.1506, -129.9795),  # alaska
#'                      ylim=c(51.2097, 71.4410))
#' gg <- gg + theme_map()
#' gg
#' }
native_areas <- function(cb = FALSE, detailed = TRUE, ...) {

  if (detailed == FALSE) {
    cb = TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
  }

  if (cb == TRUE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_aiannh_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2015/AIANNH/tl_2015_us_aiannh.zip"

  }

  return(load_tiger(url, ...))

}

#' Download an American Indian Tribal Subdivision National shapefile into R.
#'
#' Definition from the US Census Bureau: "American Indian Tribal Subdivisions (AITS) are legally defined
#' administrative subdivisions of federally
#' recognized American Indian reservations and/or off-reservation trust lands or Oklahoma tribal statistical
#' areas (OTSAs)."  For more information, please see the link provided.
#'
#' @param year The year for which you'd like to download data (defaults to 2015).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}).
#' @family native/tribal geometries functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' trib <- tribal_subdivisions_national()
#' leaflet(trib) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5)
#' }
tribal_subdivisions_national <- function(year = 2015, ...) {

  if (year == 2015) {

    url <- "http://www2.census.gov/geo/tiger/TIGER2015/AITSN/tl_2015_us_aitsn.zip"

  } else {

    url <- paste0("http://www2.census.gov/geo/tiger/TIGER",
                  as.character(year),
                  "/AITS/tl_",
                  as.character(year),
                  "_us_aitsn.zip")

  }

  return(load_tiger(url, ...))

}

#' Download an Alaska Native Regional Corporation shapefile into R.
#'
#' From the US Census Bureau: "ANRCs are corporations created according to the Alaska Native Claims Settlement Act.
#' They are organized under the laws of the State of
#' Alaska as "Regional Corporations," to conduct both the for-profit and non-profit affairs of Alaska Natives
#' within defined regions of Alaska."  For more information, please see the Census technical documentation at the
#' link provided.
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        file.  Defaults to FALSE (the most detailed TIGER/Line file)
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2015).
#' @family native/tribal geometries functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc.pdf}
#' @export
alaska_native_regional_corporations <- function(cb = FALSE, ...) {

  if (cb == TRUE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_02_anrc_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2015/ANRC/tl_2015_02_anrc.zip"

  }

  return(load_tiger(url, ...))

}

#' Download a Tribal block groups shapefile into R.
#'
#' From the US Census Bureau: "Tribal block groups are subdivisions of a tribal
#' census tract. Tribal block groups were defined by federally
#' recognized tribal government officials in the Census Bureau's
#' Tribal Statistical Areas Program (TSAP) for
#' the 2010 Census. If a tribal government declined to participate in TSAP, the Census Bureau delineated
#' tribal block groups on the American Indian reservation and/or off-reservation trust land (ORTL). Tribal
#' block groups are intended to generally contain between 600 and 3000 persons or between 240 and
#' 1200 housing units. Many American Indian reservations and ORTLs have less than the minimum
#' population thresholds for more than one tribal block group and in those cases one tribal block group was
#' delineated that covers the entire American Indian reservation and/or ORTL. Unlike standard block
#' groups, the cluster of blocks that comprises each tribal block group will not necessarily begin with the
#' same first number of their 4-character census block number,
#' but may contain blocks from several different
#' standard census block groups."  For more information, please see the link provided.
#'
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2015).
#' @family native/tribal geometries functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' trib <- tribal_block_groups()
#' leaflet(trib) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5)
#' }
tribal_block_groups <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2015/TBG/tl_2015_us_tbg.zip"

  return(load_tiger(url, ...))

}

#' Download a Tribal Census tract shapefile into R.
#'
#' From the US Census Bureau: "Tribal census tracts are relatively small statistical
#' subdivisions of an American Indian reservation and/or
#' off-reservation trust land (ORTL) and were defined by
#' federally recognized tribal government officials in
#' the Census Bureau's Tribal Statistical Areas Program (TSAP) for the 2010 Census. If a tribal government
#' declined to participate in TSAP, the Census Bureau delineated tribal census tracts on the American
#' Indian reservation and/or ORTL. Tribal census tracts are conceptually similar and equivalent to standard
#' census tracts. Unlike standard census tracts, however, tribal census tracts may cross state, county, and
#' standard census tract boundaries." For more information, please view the link provided.
#'
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2015).
#' @family native/tribal geometries functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2015/TGRSHP2015_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' trib <- tribal_census_tracts()
#' leaflet(trib) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5)
#' }
tribal_census_tracts <- function(...) {

  url <- "http://www2.census.gov/geo/tiger/TIGER2015/TTRACT/tl_2015_us_ttract.zip"

  return(load_tiger(url, ...))

}
