#' Download shapefile for all states into R
#'
#' States and Equivalent Entities are the primary governmental divisions of the
#' United States.  In addition to the 50 states, the Census Bureau treats the
#' District of Columbia, Puerto Rico, American Samoa, the Commonwealth of the
#' Northern Mariana Islands, Guam, and the U.S. Virgin Islands as the statistical
#' equivalents of states for the purpose of data presentation.
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        states file.  Defaults to FALSE (the most detailed TIGER/Line file)
#' @param resolution The resolution of the cartographic boundary file (if cb == TRUE).
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @param year the year of the data download (defaults to 2019)
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{class}, which can be set to \code{"sf"} (the default) or \code{"sp"} to
#'        request sf or sp class objects, and \code{refresh}, which specifies whether or
#'        not to re-download shapefiles (defaults to \code{FALSE}).
#' @export
#' @family general area functions
#' @seealso \url{https://www2.census.gov/geo/pdfs/reference/GARM/Ch4GARM.pdf}
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' states <- states(cb = TRUE)
#'
#' leaflet(states) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5) %>%
#'   setView(-98.5795, 39.8282, zoom=3)
#' }
states <- function(cb = FALSE, resolution = '500k', year = NULL, ...) {

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
  }

  if (is.null(year)) {

    year = getOption("tigris_year", 2019)

  }

  cyear <- as.character(year)


  if (cb == TRUE) {

    if (year %in% c(1990, 2000)) {

      suf <- substr(as.character(year), 3, 4)

      url <- sprintf("https://www2.census.gov/geo/tiger/PREVGENZ/st/st%sshp/st99_d%s_shp.zip",
                     suf, suf)

    } else if (year == 2010) {

      url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_040_00_%s.zip",
                     resolution)

    } else {

      if (year > 2013) {

        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_state_%s.zip",
                       cyear, cyear, resolution)

      } else {

        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_state_%s.zip",
                       cyear, cyear, resolution)
      }

    }

  } else {

    if (year == 1990) stop("Please specify `cb = TRUE` to get 1990 data.", call. = FALSE)

    if (year %in% c(2000, 2010)) {

      suf <- substr(cyear, 3, 4)

      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/STATE/%s/tl_2010_us_state%s.zip",
                     cyear, suf)

    } else {

      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/STATE/tl_%s_us_state.zip",
                     cyear, cyear)

    }

  }

  st <- load_tiger(url, tigris_type="state", ...)

  # Dissolve polygons for 1990 and 2000 CB
  if (cb && year %in% c(1990, 2000)) {
    sclass <- class(st)
    if (!any(sclass == "sf")) {
      st <- st_as_sf(st)
    }
    if (year == 1990) {
      st <- st %>%
        group_by(.data$ST) %>%
        summarize(AREA = sum(.data$AREA),
                  PERIMETER = sum(.data$PERIMETER),
                  ST99_D90_ = first(.data$ST99_D90_),
                  ST99_D90_I = first(.data$ST99_D90_I),
                  NAME = first(.data$NAME)) %>%
        st_cast("MULTIPOLYGON")
    } else if (year == 2000) {
      st <- st %>%
        group_by(.data$STATE) %>%
        summarize(AREA = sum(.data$AREA),
                  PERIMETER = sum(.data$PERIMETER),
                  ST99_D00_ = first(.data$ST99_D00_),
                  ST99_D00_I = first(.data$ST99_D00_I),
                  NAME = first(.data$NAME),
                  LSAD = first(.data$LSAD),
                  REGION = first(.data$REGION),
                  DIVISION = first(.data$DIVISION),
                  LSAD_TRANS = first(.data$LSAD_TRANS)) %>%
        st_cast("MULTIPOLYGON")
    }
    if (any(sclass == "SpatialPolygonsDataFrame")) {
      st <- as(st, "Spatial")
    }
  }

  return(st)

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
