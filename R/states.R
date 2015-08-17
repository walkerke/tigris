#' Download shapefile for all states into R
#'
#' States and Equivalent Entities are the primary governmental divisions of the
#' United States.  In addition to the 50 states, the Census Bureau treats the
#' District of Columbia, Puerto Rico, American Samoa, the Commonwealth of the
#' Northern Mariana Islands, Guam, and the U.S. Virgin Islands as the statistical
#' equivalents of states for the purpose of data presentation.
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        states file.  Defaults to TRUE (the most detailed TIGER/Line file)
#' @export
#' @family general area functions
#' @seealso \url{https://www.census.gov/geo/reference/gtc/gtc_state.html}
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' states <- states(detailed=FALSE)
#'
#' leaflet(states) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5) %>%
#'   setView(-98.5795, 39.8282, zoom=3)
#' }
states <- function(detailed = TRUE, ...) {

  if (detailed == FALSE) {
    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_state_500k.zip"
  } else {
    url <- "http://www2.census.gov/geo/tiger/TIGER2014/STATE/tl_2014_us_state.zip"
  }

  return(load_tiger(url, tigris_type="state", ...))

}
#' Filter a \code{states} Spatial object for only those states matching the
#' contents of the \code{state} vector.
#'
#' @param states object returned from a call to \code{states}
#' @param state a vector of full state names. The function performs the
#'        comparison in a case-insensitive manner.
#' @export
#' @examples \dontrun{
#' states() %>% filter_state("south")
#' }
filter_state <- function(states, state) {
  if (is_tigris(states) & tigris_type(states) == "state") {
    tmp <- states[tolower(states$NAME) %in% tolower(state),]
    attr(tmp, "tigris") <- "state"
    return(tmp)
  }
}

#' Find states matching a term in a \code{state} object
#'
#' This is just shorthand for
#' \code{grep(term, list_states(states), value=TRUE, ignore.case=TRUE)}
#'
#' @param states object returned from a call to \code{state}
#' @param term equivalent to the \code{pattern} argument of \code{grep}
#' @export
#' @examples \dontrun{
#' states() %>% grep_state("north")
#' }
grep_state <- function(states, term) {
  if (is_tigris(states) & tigris_type(states) == "state") {
    grep(term, list_states(states), value=TRUE, ignore.case=TRUE)
  }
}

#' Return a list of all the states in a \code{state} object
#'
#' @param states object returned from a call to \code{state}
#' @param sorted return the list sorted or in the order found in the shapefile?
#' @export
#' @examples \dontrun{
#' states() %>% list_states()
#' }
list_states <- function(states, sorted=TRUE) {
  if (is_tigris(states) & tigris_type(states) == "state") {
    if (sorted) return(sort(states@data$NAME))
    return(states@data$NAME)
  }
}
