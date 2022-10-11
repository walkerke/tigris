#' Download a congressional districts shapefile into R
#'
#' Description from the US Census Bureau (see link for source):
#' Congressional districts are the 435 areas from which members are elected to the U.S. House of Representatives. After the apportionment of congressional seats among the states, which is based on decennial census population counts, each state with multiple seats is responsible for establishing congressional districts for the purpose of electing representatives. Each congressional district is to be as equal in population to all other congressional districts in a state as practicable. The boundaries and numbers shown for the congressional districts are those specified in the state laws or court orders establishing the districts within each state.
#'
#' Congressional districts for the 108th through 112th sessions were established by the states based on the result of the 2000 Census. Congressional districts for the 113th through 116th sessions were established by the states based on the result of the 2010 Census. Boundaries are effective until January of odd number years (for example, January 2015, January 2017, etc.), unless a state initiative or court ordered redistricting requires a change. All states established new congressional districts in 2011-2012, with the exception of the seven single member states (Alaska, Delaware, Montana, North Dakota, South Dakota, Vermont, and Wyoming).
#'
#' The current default in tigris reflects boundaries for the 116th Congress, which is available as of February 2021 for years 2018 through 2021.  Older congressional district boundaries back to 2011 can be obtained by supplying the appropriate year.
#'
#' @param state The two-digit FIPS code (string) of the state you want, or a
#'        vector of codes if you want multiple states. Can also be state name
#'        or state abbreviation.  If \code{NULL} (the default), returns the entire United States.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @param resolution The resolution of the cartographic boundary file (if cb == TRUE).
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family legislative district functions
#' @seealso \url{https://www.census.gov/programs-surveys/geography/guidance/geo-areas/congressional-dist.html}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' cd116 <- congressional_districts(cb = TRUE, resolution = '20m')
#'
#' leaflet(cd116) %>%
#'    addTiles() %>%
#'    addPolygons()
#' }
congressional_districts <- function(state = NULL, cb = FALSE, resolution = '500k', year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2021)

    message(sprintf("Retrieving data for the year %s", year))

  }

  if (year < 2013 && cb == TRUE) {
    stop("`cb = TRUE` for congressional districts is unavailable prior to 2013. Regular TIGER/Line files are available for 2010 through 2010 with `cb = FALSE`",
         call. = FALSE)
  }

  if (year %in% 2018:2022) {
    congress <- "116"
  } else if (year %in% 2016:2017) {
    congress <- "115"
  } else if (year %in% 2014:2015) {
    congress <- "114"
  } else if (year == 2013) {
    congress <- "113"
  } else if (year %in% 2011:2012) {
    congress <- "112"
  } else if (year == 2010) {
    congress <- "111"
  }

  if (year < 2010) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2010.", fname)

    stop(msg, call. = FALSE)

  }

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
  }

  cyear <- as.character(year)

  if (cb == TRUE) {

    url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_cd%s_%s.zip",
                   cyear, cyear, congress, resolution)

    if (year == 2013) url <- gsub("shp/", "", url)

  } else {

    url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/CD/tl_%s_us_cd%s.zip",
                   cyear, cyear, congress)

  }

  cds <- load_tiger(url, tigris_type="congressional_districts", ...)

  state <- unlist(sapply(state, validate_state, USE.NAMES=FALSE))

  if (!is.null(state)) {

    cds <- cds[cds$STATEFP %in% state,]

  }

  return(cds)

}

#' Download a state legislative districts shapefile into R - upper or lower
#'
#' This function allows you to download boundaries for state legislatures into R.
#' Generally, state legislatures are comprised of an "upper" house, which is
#' typically referred to as the Senate, and a "lower" house, which is often (but
#' not exclusively) referred to as the House.  The exception is Nebraska, which
#' has a unicameral state legislature.
#'
#' @param state The two-digit FIPS code (string) of the state. Can also be state
#'        name or abbreviation (case-insensitive). When \code{NULL} and combined with
#'        \code{cb = TRUE}, a national dataset of state legislative districts will be returned.
#' @param house Specify here whether you want boundaries for the \code{upper} or
#'        \code{lower} house.  Defaults to \code{upper}.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family legislative district functions
#' @seealso \url{https://www.census.gov/programs-surveys/geography/guidance/geo-areas/state-legis-dist.html}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' leg <- state_legislative_districts("Maine", "lower", cb = TRUE)
#'
#' leaflet(leg) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5)
#' }
state_legislative_districts <- function(state= NULL, house = "upper",
                                        cb = FALSE, year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2021)

    message(sprintf("Retrieving data for the year %s", year))

  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

  if (is.null(state)) {
    if (year > 2018 && cb == TRUE) {
      state <- "us"
      message("Retrieving state legislative districts for the entire United States")
    } else {
      stop("A state must be specified for this year/dataset combination.",
           call. = FALSE)
    }
  } else {
    state <- validate_state(state)

    if (is.null(state)) stop("Invalid state", call.=FALSE)
  }

  if (!house %in% c("upper", "lower"))
    stop("Must specify 'upper' or 'lower' for 'house' parameter", call.=FALSE)

  if (house == "lower" & state == "31") { # Nebraska

    type <- "sldu"

  } else if (house == "lower") {

    type <- "sldl"

  } else {

    type <- "sldu"

  }

  cyear <- as.character(year)


  if (cb == TRUE) {

    if (year == 2010) {
      if (type == "sldu") {
        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_610_u2_500k.zip",
                       state)
      } else if (type == "sldl") {
        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_620_l2_500k.zip",
                       state)
      }
    }

    url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_%s_500k.zip",
                   cyear, cyear, state, type)

    if (year == 2013) url <- gsub("shp/", "", url)

  } else {

    if (year %in% c(2000, 2010)) {
      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/%s/%s/tl_2010_%s_%s%s.zip",
                     toupper(type), cyear, state, type, substr(cyear, 3, 4))
    } else {
      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/%s/tl_%s_%s_%s.zip",
                     cyear, toupper(type), cyear, state, type)
    }

  }

  return(load_tiger(url, tigris_type="state_legislative_districts", ...))

}

#' Download a voting districts shapefile into R
#'
#' Obtain feature geometry for 2020 voting districts, which align with voting districts for the
#' 2020 PL-94171 redistricting data from the US Census Bureau.
#'
#' The US Census Bureau describes \emph{voting districts} as follows:
#' Voting district (VTD) is a generic term adopted by the Bureau of the Census
#' to include the wide variety of small polling areas, such as election districts,
#' precincts, or wards, that State and local governments create for the purpose
#' of administering elections. Some States also use groupings of these entities
#' to define their State and local legislative districts, as well as the districts they
#' define for election of members to the U.S. House of Representatives. In a
#' nationwide cooperative program for the 1980 census, the Census Bureau
#' gave States the opportunity to request use of these election precinct boundaries as the boundaries of #' census enumeration districts (EDs) or, in some areas, census blocks.
#'
#' Support for voting districts in tigris 1.5 and higher is aligned with the 2020 PL redistricting
#' data.  The argument \code{cb = FALSE} retrieves voting districts from the TIGER/Line PL
#' shapefiles.  A generalized version from the cartographic boundary dataset is available with the
#' argument \code{cb = TRUE}.
#'
#' @param state The state for which you'd like to retrieve data.  Can be a state name,
#'        state abbreviation, or FIPS code. When \code{NULL} and combined with
#'        \code{cb = TRUE}, a national dataset of voting districts will be returned.
#' @param county The county for which you are requesting data.  Can be a county name or
#'               FIPS code.  If \code{NULL} (the default), data for the entire state will
#'               be returned.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family legislative district functions
#' @seealso \url{https://www2.census.gov/geo/pdfs/reference/GARM/Ch14GARM.pdf}
#' @export
#' @examples \dontrun{#'
#' library(tigris)
#'
#' ia <- voting_districts("Iowa")
#'
#' plot(ia$geometry)
#'
#' }
voting_districts <- function(state = NULL, county = NULL, cb = FALSE, year = 2020, ...) {

  if (year != 2020 && cb == TRUE) {
    stop("Cartographic boundary voting districts files are only available for 2020.",
         call. = FALSE)
  }

  if (is.null(state)) {
    if (year > 2018 && cb == TRUE) {
      state <- "us"
      message("Retrieving voting districts for the entire United States")
    } else {
      stop("A state must be specified for this year/dataset combination.",
           call. = FALSE)
    }
  } else {
    state <- validate_state(state)

    if (is.null(state)) stop("Invalid state", call.=FALSE)
  }

  if (cb) {

    url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_%s_vtd_500k.zip", state)

    vtds <- load_tiger(url, tigris_type = "voting_districts", ...)

    if (is.null(county)) {
      return(vtds)
    } else {
      county = validate_county(state, county)
      vtds_sub <- vtds[vtds$COUNTYFP20 == county,]
      return(vtds_sub)
    }

  } else {

    if (year == 2012) {

      url <- paste0("https://www2.census.gov/geo/tiger/TIGER2012/VTD/tl_2012_",
                    state,
                    "_vtd10.zip")
    } else {
      if (!is.null(county)) {

        county <- validate_county(state, county)

        url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2020PL/LAYER/VTD/2020/tl_2020_%s%s_vtd20.zip",
                       state, county)
      } else {
        url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2020PL/LAYER/VTD/2020/tl_2020_%s_vtd20.zip",
                       state)
      }
    }

    return(load_tiger(url, tigris_type = 'voting_districts', ...))

  }
}


