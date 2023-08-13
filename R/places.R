#' Download Shapefiles which include both incorporated places (legal entities) and census designated places (statistical entities) into R
#'
#' An incorporated place provides governmental functions for a concentration of people. Incorporated places may extend across county and county subdivision boundaries, but never across state boundaries. An incorporated place usually is a city, town, village, or borough, but can have other legal descriptions. CDPs are the statistical counterparts of incorporated places. CDPs are settled concentrations of population that are identifiable by name but not legally incorporated under the laws of the state in which the CDPs are located. For more information, read appropriate section on "Places" for the correct vintage (year) of your data in the TIGER/Line Shapefiles and TIGER/Line Files Technical Documentation available as of this writing here: <https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/tiger-geo-line.html>.
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation. When `NULL` and combined with
#'        `cb = TRUE`, a national dataset of places will be returned for years
#'        2019 and later.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family general area functions
#' @seealso <https://www2.census.gov/geo/pdfs/reference/GARM/Ch9GARM.pdf>
#' @export
places <- function(state = NULL, cb = FALSE, year = NULL, ...) {

  if (length(state) > 1) {
    p <- lapply(state, function(x) {
      places(state = x, cb = cb, year = year, ...)
    }) %>%
      rbind_tigris()

    return(p)
  }

  year <- set_tigris_year(year)

  if (is.null(state)) {
    if (year > 2018 && cb == TRUE) {
      state <- "us"
      message("Retrieving Census-designated places for the entire United States")
    } else {
      stop("A state must be specified for this year/dataset combination.",
           call. = FALSE)
    }
  } else {
    state <- validate_state(state, allow_null = FALSE)
  }

  if (cb == TRUE) {
    url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_place_500k.zip",
                   year, year, state)
  } else {
    url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/PLACE/tl_%s_%s_place.zip",
                   year, year, state)
  }

  return(load_tiger(url, tigris_type="place", ...))

}

#' Filter a `places` Spatial object for only those places matching the
#' contents of the `place` vector.
#'
#' @param places object returned from a call to `places`
#' @param place a vector of full place names. The function performs the
#'        comparison in a case-insensitive manner.
#' @export
#' @examples \dontrun{
#' places("Maine") %>% filter_place("berwick")
#' }
filter_place <- function(places, place) {
  if (is_tigris(places) & tigris_type(places) == "place") {
    tmp <- places[tolower(places$NAME) %in% tolower(place),]
    attr(tmp, "tigris") <- "place"
    return(tmp)
  }
}

#' Find places matching a term in a `places` object
#'
#' This is just shorthand for
#' `grep(term, list_places(places), value=TRUE, ignore.case=TRUE)`
#'
#' @param places object returned from a call to `places`
#' @param term equivalent to the `pattern` argument of `grep`
#' @export
#' @examples \dontrun{
#' places("Maine") %>% grep_place("south")
#' }
grep_place <- function(places, term) {
  if (is_tigris(places) & tigris_type(places) == "place") {
    grep(term, list_places(places), value=TRUE, ignore.case=TRUE)
  }
}

#' Return a list of all the places in a `places` object
#'
#' @param places object returned from a call to `places`
#' @param sorted return the list sorted or in the order found in the shapefile?
#' @export
#' @examples \dontrun{
#' places("Maine") %>% list_places()
#' }
list_places <- function(places, sorted=TRUE) {
  if (is_tigris(places) & tigris_type(places) == "place") {
    if (sorted) return(sort(places$NAME))
    return(places$NAME)
  }
}
