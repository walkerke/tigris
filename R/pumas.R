#' Download a Public Use Microdata Area (PUMA) shapefile into R
#'
#' Public use microdata areas (PUMAs) are decennial census areas that have been
#' defined for the tabulation and dissemination of Public Use Microdata Sample
#' (PUMS) data, American Community Survey (ACS) data, and ACS period estimates.
#' For the 2010 Census, the State Data Centers (SDCs) in each state, the
#' District of Columbia, and the Commonwealth of Puerto Rico were given the
#' opportunity to delineate PUMAs within their state or
#' statistically equivalent entity. All PUMAs must nest within states and have
#' a minimum population threshold of 100,000 persons. 2010 PUMAs were built on
#' census tracts and cover the entirety of the United States, Puerto Rico,
#' Guam, and the U.S. Virgin Islands. Because they do not meet the minimum
#' population requirement, the Commonwealth of the Northern Mariana Islands
#' and American Samoa do not contain any 2010 PUMAs.
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation. When `NULL` and combined with
#'        `cb = TRUE`, a national dataset of PUMAs will be returned when
#'        `year = 2019` only.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        states file.  Defaults to FALSE (the most detailed TIGER/Line file)
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @export
#' @family general area functions
#' @seealso <https://www.census.gov/programs-surveys/geography/guidance/geo-areas/pumas.html>
#' @examples \dontrun{
#' library(tigris)
#'
#' us_states <- unique(fips_codes$state)[1:51]
#'
#' continental_states <- us_states[!us_states %in% c("AK", "HI")]
#' pumas_list <- lapply(continental_states, function(x) {
#'   pumas(state = x, cb = TRUE, year = 2017)
#'   })
#'
#' us_pumas <- rbind_tigris(pumas_list)
#'
#' plot(us_pumas$geometry)
#' }
pumas <- function(state = NULL, cb = FALSE, year = NULL, ...) {

  year <- set_tigris_year(year)

  if (is.null(state)) {
    if (year == 2019 && cb == TRUE) {
      state <- "us"
      message("Retrieving PUMAs for the entire United States")
    } else {
      stop("A state must be specified for this year/dataset combination.",
           call. = FALSE)
    }
  } else {
    state <- validate_state(state, allow_null = FALSE)
  }

  if (year > 2021) {
    suf <- "20"
  } else {
    suf <- "10"
  }

  if (cb == TRUE) {

    if (year > 2019) {
      stop("Cartographic boundary PUMAs are not yet available for years after 2019. Use the argument `year = 2019` instead to request your data.", call. = FALSE)
    }

    url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_puma10_500k.zip",
                   year, year, state)

    if (year == 2013) url <- gsub("shp/", "", url)


  } else {

    url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/PUMA/tl_%s_%s_puma%s.zip",
                   year, year, state, suf)
  }

  pm <- load_tiger(url, tigris_type = "puma", ...)

  attr(pm, "tigris") <- "puma"

  return(pm)

}



