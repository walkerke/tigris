#' Download an area water shapefile into R
#'
#' @param state The two-digit FIPS code of the state of the county you'd like to download the water features for
#' @param county The three-digit FIPS code of the county you'd like the water features for
#' @export

area_water <- function(state, county) {

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/AREAWATER/tl_2014_",
                state,
                county,
                "_areawater.zip")

  awtr <- load_tiger(url)

  awtr

}


#' Download an linear water shapefile into R
#'
#' @param state The two-digit FIPS code of the state of the county you'd like to download the water features for
#' @param county The three-digit FIPS code of the county you'd like the water features for
#' @export

linear_water <- function(state, county) {

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/LINEARWATER/tl_2014_",
                state,
                county,
                "_linearwater.zip")

  lwtr <- load_tiger(url)

  lwtr

}


#' Download a shapefile of the US coastline into R
#'
#' @export

coastline <- function() {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/COAST/tl_2014_us_coastline.zip"

  coast <- load_tiger(url)

  coast

}
