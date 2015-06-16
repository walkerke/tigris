#' Download a roads shapefile into R
#'
#' @param state The two-digit FIPS code of the state of the county you'd like to download the roads for
#' @param county The three-digit FIPS code of the county you'd like the roads for
#' @export

roads <- function(state, county) {

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/ROADS/tl_2014_",
                state,
                county,
                "_roads.zip")

  rds <- load_tiger(url)

  rds

}

#' Download a national rails shapefile into R
#'
#' @export


rails <- function() {

  url <- "http://www2.census.gov/geo/tiger/TIGER2014/RAILS/tl_2014_us_rails.zip"

  rls <- load_tiger(url)

  rls

}
