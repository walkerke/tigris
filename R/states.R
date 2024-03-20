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
#' @param resolution The resolution of the cartographic boundary file (if `cb = TRUE`).
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @export
#' @family general area functions
#' @seealso <https://www2.census.gov/geo/pdfs/reference/GARM/Ch4GARM.pdf>
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

  check_tigris_resolution(resolution)

  year <- set_tigris_year(year)

  if (cb) {

    if (year %in% c(1990, 2000)) {

      suf <- substr(year, 3, 4)

      url <- url_tiger("PREVGENZ/st/st%sshp/st99_d%s_shp", suf, suf)

    } else if (year == 2010) {

      url <- url_tiger("GENZ2010/gz_2010_us_040_00_%s", resolution)

    } else {

      if (year > 2013) {

        url <- url_tiger("GENZ%s/shp/cb_%s_us_state_%s", year, year, resolution)

      } else {

        url <- url_tiger("GENZ%s/shp/cb_%s_us_state_%s", year, year, resolution)
      }

    }

  } else {

    check_cb_year(year = year, error_year = 1990)

    if (year %in% c(2000, 2010)) {

      suf <- substr(year, 3, 4)

      url <- url_tiger("TIGER2010/STATE/%s/tl_2010_us_state%s", year, suf)

    } else {

      url <- url_tiger("TIGER%s/STATE/tl_%s_us_state", year, year)

    }

  }

  st <- load_tiger(url, tigris_type = "state", ...)

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
#' Filter a `states` Spatial object for only those states matching the
#' contents of the `state` vector.
#'
#' @param states object returned from a call to `states`
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

#' Find states matching a term in a `state` object
#'
#' This is just shorthand for
#' `grep(term, list_states(states), value=TRUE, ignore.case=TRUE)`
#'
#' @param states object returned from a call to `state`
#' @param term equivalent to the `pattern` argument of `grep`
#' @export
#' @examples \dontrun{
#' states() %>% grep_state("north")
#' }
grep_state <- function(states, term) {
  if (is_tigris(states) & tigris_type(states) == "state") {
    grep(term, list_states(states), value=TRUE, ignore.case=TRUE)
  }
}

#' Return a list of all the states in a `state` object
#'
#' @param states object returned from a call to `state`
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
