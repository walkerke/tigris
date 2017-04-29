#' Download a Census-designated places shapefile into R
#'
#' Census Designated Places (CDPs) are the statistical counterparts of
#' incorporated places, and are delineated to provide data for settled
#' concentrations of population that are identifiable by name but are
#' not legally incorporated under the laws of the state in which they
#' are located.
#'
#' The boundaries usually are defined in cooperation with local or tribal
#' officials and generally updated prior to each decennial census.
#'
#' These boundaries, which usually coincide with visible features or the
#' boundary of an adjacent incorporated place or another legal entity boundary,
#' have no legal status, nor do these places have officials elected to serve
#' traditional municipal functions.
#'
#' CDP boundaries may change from one decennial census to the next with changes
#' in the settlement pattern; a CDP with the same name as in an earlier census
#' does not necessarily have the same boundary.
#'
#' CDPs must be contained within a single state and may not extend into an
#' incorporated place.
#'
#' There are no population size requirements for CDPs.
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @param year the data year (defaults to 2015).
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}).
#' @family general area functions
#' @seealso \url{https://www.census.gov/geo/reference/gtc/gtc_place.html}
#' @export
places <- function(state, cb = FALSE, year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2015)

  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  cyear <- as.character(year)

  if (cb == TRUE) {
    url <- sprintf("http://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_place_500k.zip",
                   cyear, cyear, state)
  } else {
    url <- sprintf("http://www2.census.gov/geo/tiger/TIGER%s/PLACE/tl_%s_%s_place.zip",
                   cyear, cyear, state)
  }

  return(load_tiger(url, tigris_type="place", ...))

}

#' Filter a \code{places} Spatial object for only those places matching the
#' contents of the \code{place} vector.
#'
#' @param places object returned from a call to \code{places}
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

#' Find places matching a term in a \code{places} object
#'
#' This is just shorthand for
#' \code{grep(term, list_places(places), value=TRUE, ignore.case=TRUE)}
#'
#' @param places object returned from a call to \code{places}
#' @param term equivalent to the \code{pattern} argument of \code{grep}
#' @export
#' @examples \dontrun{
#' places("Maine") %>% grep_place("south")
#' }
grep_place <- function(places, term) {
  if (is_tigris(places) & tigris_type(places) == "place") {
    grep(term, list_places(places), value=TRUE, ignore.case=TRUE)
  }
}

#' Return a list of all the places in a \code{places} object
#'
#' @param places object returned from a call to \code{places}
#' @param sorted return the list sorted or in the order found in the shapefile?
#' @export
#' @examples \dontrun{
#' places("Maine") %>% list_places()
#' }
list_places <- function(places, sorted=TRUE) {
  if (is_tigris(places) & tigris_type(places) == "place") {
    if (sorted) return(sort(places@data$NAME))
    return(places@data$NAME)
  }
}
