#' Download a US Counties shapefile into R, and optionally subset by state
#'
#' The primary legal divisions of most states are termed counties. In Louisiana,
#' these divisions are known as parishes.  In Alaska, which has no counties,
#' the equivalent entities are the organized boroughs, city and boroughs,
#' municipalities, and census areas; the latter of which are delineated
#' cooperatively for statistical purposes by the state of Alaska and the
#' Census Bureau.  In four states (Maryland, Missouri, Nevada, and Virginia),
#' there are one or more incorporated places that are independent of any county
#' organization and thus constitute primary divisions of their states.  These
#' incorporated places are known as independent cities and are treated as
#' equivalent entities for purposes of data presentation.  The District of
#' Columbia and Guam have no primary divisions, and each area is considered
#' an equivalent entity for purposes of data presentation.  All of the counties
#' in Connecticut and Rhode Island and nine counties in Massachusetts were
#' dissolved as functioning governmental entities; however, the Census Bureau
#' continues to present data for these historical entities in order to provide
#' comparable geographic units at the county level of the geographic hierarchy
#' for these states and represents them as nonfunctioning legal entities in
#' data products.  The Census Bureau treats the following entities as
#' equivalents of counties for purposes of data presentation: municipios in
#' Puerto Rico, districts and islands in American Samoa, municipalities in the
#' Commonwealth of the Northern Mariana Islands, and islands in the U.S.
#' Virgin Islands.  Each county or statistically equivalent entity is assigned
#' a three-character numeric Federal Information Processing Series (FIPS) code
#' based on alphabetical sequence that is unique within state and an
#' eight-digit National Standard feature identifier.
#'
#' @param state The two-digit FIPS code (string) of the state you want, or a
#'        vector of codes if you want multiple states. Can also be state name
#'        or state abbreviation.
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        counties file.  Defaults to TRUE (the most detailed TIGER file).
#' @export
#' @family general area functions
#' @seealso \url{https://www.census.gov/geo/reference/gtc/gtc_cou.html}
#' @examples \dontrun{
#' library(tigris)
#' library(ggplot2)
#' library(ggthemes)
#'
#' me <- counties("Maine", detailed=FALSE)
#' me_map <- fortify(me)
#'
#' gg <- ggplot()
#' gg <- gg + geom_map(data=me_map, map=me_map,
#'                     aes(x=long, y=lat, map_id=id),
#'                     color="black", fill="white", size=0.25)
#' gg <- gg + coord_map()
#' gg <- gg + theme_map()
#' gg
#' }
counties <- function(state = NULL, detailed = TRUE) {

  if (detailed == FALSE) {

    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_500k.zip"

  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/COUNTY/tl_2014_us_county.zip"

  }

  ctys <- load_tiger(url, tigris_type="county")

  state <- unlist(sapply(state, validate_state, USE.NAMES=FALSE))

  if (!is.null(state)) {
    return(ctys[ctys$STATEFP %in% state,])
  } else {
    return(ctys)
  }

}

#' Download a Census tracts shapefile into R, and optionally subset by county
#'
#' Census Tracts are small, relatively permanent statistical subdivisions of
#' a county or equivalent entity that are updated by local participants prior
#' to each decennial census as part of the Census Bureau's Participant
#' Statistical Areas Program. The Census Bureau delineates census tracts in
#' situations where no local participant existed or where state, local, or
#' tribal governments declined to participate. The primary purpose of census
#' tracts is to provide a stable set of geographic units for the presentation
#' of statistical data.
#'
#' Census tracts generally have a population size between 1,200 and 8,000 people,
#' with an optimum size of 4,000 people. A census tract usually covers a
#' contiguous area; however, the spatial size of census tracts varies widely
#' depending on the density of settlement.Census tract boundaries are
#' delineated with the intention of being maintained over a long time so that
#' statistical comparisons can be made from census to census. Census tracts
#' occasionally are split due to population growth or merged as a result of
#' substantial population decline.
#'
#' Census tract boundaries generally follow visible and identifiable features.
#' They may follow nonvisible legal boundaries, such as minor civil division
#' (MCD) or incorporated place boundaries in some states and situations, to
#' allow for census-tract-to-governmental-unit relationships where the
#' governmental boundaries tend to remain unchanged between censuses.  State and
#' county boundaries always are census tract boundaries in the standard census
#' geographic hierarchy. Tribal census tracts are a unique geographic entity
#' defined within federally recognized American Indian reservations and
#' off-reservation trust lands and can cross state and county boundaries.
#' Tribal census tracts may be completely different from the census tracts
#' and block groups defined by state and county
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param county The three-digit FIPS code (string) of the county you'd like to
#'        subset for, or a vector of FIPS codes if you desire multiple counties
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        tracts file.  Defaults to TRUE (the most detailed TIGER/Line file)
#' @family general area functions
#' @seealso \url{https://www.census.gov/geo/reference/gtc/gtc_ct.html}
#' @export
tracts <- function(state, county = NULL, detailed = TRUE) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (detailed == FALSE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_",
                  state,
                  "_tract_500k.zip")

  } else {

    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/TRACT/tl_2014_",
                  state,
                  "_tract.zip")
  }

  trcts <- load_tiger(url, tigris_type="tract")

  if (!is.null(county)) trcts <- trcts[trcts$COUNTYFP %in% county, ]

  attr(trcts, "tigris") <- "tract"

  return(trcts)

}

#' Download a unified school district shapefile into R
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @family general area functions
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' schools <- school_districts("Maine")
#'
#' leaflet(schools) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolygons(fillColor = "white",
#'               color = "black",
#'               weight = 0.5)
#' }
school_districts <- function(state) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/UNSD/tl_2014_", state, "_unsd.zip")

  return(load_tiger(url, tigris_type="school"))

}

#' Download a Census block groups shapefile into R, and optionally subset by county
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param county The three-digit FIPS code (string) of the county you'd like to
#'        subset for, or a vector of FIPS codes if you desire multiple counties
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        file.  Defaults to TRUE (the most detailed TIGER/Line file)
#' @family general area functions
#' @export
block_groups <- function(state, county = NULL, detailed = TRUE) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (detailed == FALSE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_",
                  state,
                  "_bg_500k.zip")

  } else {

    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/BG/tl_2014_",
                  state,
                  "_bg.zip")
  }

  bgs <- load_tiger(url, tigris_type="block")

  if (!is.null(county)) bgs <- bgs[bgs$COUNTYFP %in% county, ]

  attr(bgs, "tigris") <- "block"

  return(bgs)

}

#' Download a Zip Code Tabulation Area (ZCTA) shapefile into R
#'
#' @param detailed If detailed is set to FALSE, download a generalized (1:500k)
#'        ZCTA file.  Defaults to TRUE (the most detailed TIGER/Line file).
#'        \strong{A warning:} the detailed TIGER/Line ZCTA file is massive
#'        (around 502MB unzipped), and the generalized version is also large
#'        (64MB zipped).  Be prepared for this especially if you have a slower
#'        internet connection.
#' @param starts_with Character string specifying the beginning digits of the
#'        ZCTAs you want to return.  For example, supplying the argument
#'        \code{starts_with = "761"} will return only those ZCTAs that begin
#'        with 761.  Defaults to NULL, which will return all ZCTAs in the US.
#' @family general area functions
#' @export
zctas <- function(detailed = TRUE, starts_with = NULL) {

  if (detailed == FALSE) {
    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_zcta510_500k.zip"
  } else {
    url <- "http://www2.census.gov/geo/tiger/TIGER2014/ZCTA5/tl_2014_us_zcta510.zip"
  }

  zcta <- load_tiger(url, tigris_type="zcta")

  if (!is.null(starts_with)) zcta <- zcta[grep(paste0("^", starts_with), zcta$ZCTA5CE10), ]

  attr(zcta, "tigris") <- "zcta"

  return(zcta)

}


#' Download a Census block shapefile into R
#'
#' This function will download an entire block shapefile for a selected state
#' into R, and optionally subset by county. \strong{A warning:} Census block
#' shapefiles are often very large, especially for large states - for example, the
#' block file for Texas is 462MB zipped!  If you have a slow or unreliable internet
#' connection, or insufficient memory, this may prove burdensome given that you
#' have to first download by state and then subset.
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param county The three-digit FIPS code (string) of the county you'd like to
#'        subset for, or a vector of FIPS codes if you desire multiple counties
#' @family general area functions
#' @export
blocks <- function(state, county = NULL) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/TABBLOCK/tl_2014_",
                state,
                "_tabblock10.zip")

  blks <- load_tiger(url, tigris_type="block")

  if (!is.null(county)) blks <- blks[blks$COUNTYFP10 %in% county, ]

  attr(blks, "tigris") <- "block"

  return(blks)

}
