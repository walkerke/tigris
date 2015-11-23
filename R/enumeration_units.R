#' Download a US Counties shapefile into R, and optionally subset by state
#'
#' Description from the US Census Bureau (see link for source):
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
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        counties file.  Defaults to FALSE (the most detailed TIGER file).
#' @param resolution The resolution of the cartographic boundary file (if cb == TRUE).
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @export
#' @family general area functions
#' @seealso \url{https://www.census.gov/geo/reference/gtc/gtc_cou.html}
#' @examples \dontrun{
#' library(tigris)
#' library(ggplot2)
#' library(ggthemes)
#'
#' me <- counties("Maine", cb = TRUE)
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
counties <- function(state = NULL, cb = FALSE, resolution = '500k', detailed = TRUE, ...) {

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
  }

  if (detailed == FALSE) {
    cb = TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
    }

  if (cb == TRUE) {
    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_",
                  resolution,
                  ".zip")
  } else {

    url <- "http://www2.census.gov/geo/tiger/TIGER2014/COUNTY/tl_2014_us_county.zip"

  }

  ctys <- load_tiger(url, tigris_type="county", ...)

  state <- unlist(sapply(state, validate_state, USE.NAMES=FALSE))

  if (!is.null(state)) {
    return(ctys[ctys$STATEFP %in% state,])
  } else {
    return(ctys)
  }

}

#' Download a Census tracts shapefile into R, and optionally subset by county
#'
#' Description from the US Census Bureau (see link for source):
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
#' geographic hierarchy.
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param county The three-digit FIPS code (string) of the county you'd like to
#'        subset for, or a vector of FIPS codes if you desire multiple counties.
#'        Can also be a county name or vector of names.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        tracts file.  Defaults to FALSE (the most detailed TIGER/Line file)
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family general area functions
#' @seealso \url{https://www.census.gov/geo/reference/gtc/gtc_ct.html}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(leaflet)
#'
#' tarrant <- tracts("TX", "Tarrant", cb = TRUE)
#'
#' leaflet(tarrant) %>%
#'   addTiles() %>%
#'   addPolygons(popup = ~NAME)
#' }
tracts <- function(state, county = NULL, cb = FALSE, detailed = TRUE, ...) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (detailed == FALSE) {
    cb = TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
  }

  if (cb == TRUE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_",
                  state,
                  "_tract_500k.zip")

  } else {

    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/TRACT/tl_2014_",
                  state,
                  "_tract.zip")
  }

  trcts <- load_tiger(url, tigris_type="tract", ...)

  if (!is.null(county)) {

     county <- sapply(county, function(x) validate_county(state, x))

     trcts <- trcts[trcts$COUNTYFP %in% county, ]

  }

  attr(trcts, "tigris") <- "tract"

  return(trcts)

}

#' Download a school district shapefile into R
#'
#' From the US Census Bureau (see link for source):
#' School Districts are single-purpose administrative units within which local officials provide public
#' educational services for the area's residents. The Census Bureau obtains school district boundaries,
#' names, local education agency codes, grade ranges, and school district levels biennially from state
#' education officials. The Census Bureau collects this information for the primary purpose of providing the
#' U.S. Department of Education with annual estimates of the number of children in poverty within each
#' school district, county, and state. This information serves as the basis for the Department of Education to
#' determine the annual allocation of Title I funding to states and school districts.
#'
#' The Census Bureau creates pseudo-unified school districts for areas in which unified school districts do
#' not exist.  Additionally, elementary and secondary school districts do not exist in all states.
#' Please see the link for more information on how the Census Bureau creates the school district shapefiles.
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param type Specify whether you want to return a unified school district (the default, \code{'unified'}),
#'        an elementary school district (\code{'elementary'}), or a secondary school district (\code{'secondary'}).
#'        Please note: elementary and secondary school districts do not exist in all states
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family general area functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2014/TGRSHP2014_TechDoc.pdf}
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
school_districts <- function(state, type = 'unified', ...) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (type == 'unified') {
    type <- 'unsd'
  } else if (type == 'elementary') {
    type <- 'elsd'
  } else if (type == 'secondary') {
    type <- 'scsd'
  } else {
    stop("Invalid school district type.  Valid types are 'unified', 'elementary', and 'secondary'.", call. = FALSE)
  }

  url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/",
                toupper(type),
                "/tl_2014_",
                state,
                "_",
                type,
                ".zip")

  return(load_tiger(url, tigris_type = type, ...))

}

#' Download a Census block groups shapefile into R, and optionally subset by county
#'
#' Description from the US Census Bureau (see link for source):Standard block groups are clusters of
#' blocks within the same census tract that have the same first digit of
#' their 4-character census block number. For example, blocks 3001, 3002, 3003..., 3999 in census tract
#' 1210.02 belong to Block Group 3. Due to boundary and feature changes that occur throughout the
#' decade, current block groups do not always maintain these same block number to block group
#' relationships. For example, block 3001 might move due to a census tract boundary change but the block
#' number will not change, even if it does not still fall in block group 3. However, the GEOID for that block,
#' identifying block group 3, would remain the same in the attribute information in the TIGER/Line Shapefiles
#' because block GEOIDs are always built using the decennial geographic codes.
#'
#' Block groups delineated for the 2010 Census generally contain between 600 and 3,000 people. Most
#' block groups were delineated by local participants in the Census Bureau's Participant Statistical Areas
#' Program (PSAP). The Census Bureau delineated block groups only where a local or tribal government
#' declined to participate or where the Census Bureau could not identify a potential local participant.
#'
#' A block group usually covers a contiguous area. Each census tract contains at least one block group and
#' block groups are uniquely numbered within census tract. Within the standard census geographic
#' hierarchy, block groups never cross county or census tract boundaries, but may cross the boundaries of
#' county subdivisions, places, urban areas, voting districts, congressional districts, and American Indian,
#' Alaska Native, and Native Hawaiian areas.
#'
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param county The three-digit FIPS code (string) of the county you'd like to
#'        subset for, or a vector of FIPS codes if you desire multiple counties.
#'        Can also be a county name or vector of names.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        file.  Defaults to FALSE (the most detailed TIGER/Line file)
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family general area functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2014/TGRSHP2014_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(sp)
#'
#' benton_bgs <- block_groups("Oregon", "Benton")
#'
#' plot(benton_bgs)
#' }
block_groups <- function(state, county = NULL, cb = FALSE, detailed = TRUE, ...) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (detailed == FALSE) {
    cb <- TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
  }

  if (cb == TRUE) {

    url <- paste0("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_",
                  state,
                  "_bg_500k.zip")

  } else {

    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/BG/tl_2014_",
                  state,
                  "_bg.zip")
  }

  bgs <- load_tiger(url, tigris_type="block", ...)

  if (!is.null(county)) {

    county <- sapply(county, function(x) validate_county(state, x))

    bgs <- bgs[bgs$COUNTYFP %in% county, ]

  }

  attr(bgs, "tigris") <- "block"

  return(bgs)

}

#' Download a Zip Code Tabulation Area (ZCTA) shapefile into R
#'
#' ZIP Code Tabulation Areas (ZCTAs) are generalized areal representations of
#' United States Postal Service (USPS) ZIP Code service areas.  Please see the link provided for
#' information on how the Census Bureau creates ZCTAs, and for important information on the
#' differences between ZCTAs and ZIP Codes.
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        ZCTA file.  Defaults to FALSE (the most detailed TIGER/Line file).
#'        \strong{A warning:} the detailed TIGER/Line ZCTA file is massive
#'        (around 502MB unzipped), and the generalized version is also large
#'        (64MB zipped).  Be prepared for this especially if you have a slower
#'        internet connection.
#' @param starts_with Character vector specifying the beginning digits of the
#'        ZCTAs you want to return.  For example, supplying the argument
#'        \code{starts_with = c("75", "76")} will return only those ZCTAs that begin
#'        with 75 or 76.  Defaults to NULL, which will return all ZCTAs in the US.
#' @param detailed (deprecated) Setting detailed to FALSE returns a 1:500k cartographic boundary file.
#'        This parameter will be removed in a future release.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family general area functions
#' @seealso \url{https://www.census.gov/geo/reference/zctas.html}
#' @export
#' @examples \dontrun{
#' # Example: get ZCTAs that intersect the Memphis, TN urbanized area
#'
#' library(tigris)
#' library(rgeos)
#' library(sp)
#'
#' df <- zctas(cb = TRUE, starts_with = c("37", "38", "72"))
#'
#' uas <- urban_areas()
#'
#' memphis_ua <- uas[grep("Memphis", uas$NAME10), ]
#'
#' mem_zcta <- df[as.vector(gIntersects(df, memphis_ua, byid = TRUE)), ]
#'
#' plot(mem_zcta)
#'
#' }
zctas <- function(cb = FALSE, starts_with = NULL, detailed = TRUE, ...) {

  if (detailed == FALSE) {
    cb <- TRUE
    message("The `detailed` parameter is deprecated.  Use `cb` instead.")
  }

  if (cb == TRUE) {
    url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_zcta510_500k.zip"
  } else {
    url <- "http://www2.census.gov/geo/tiger/TIGER2014/ZCTA5/tl_2014_us_zcta510.zip"
  }

  zcta <- load_tiger(url, tigris_type="zcta", ...)

  if (!is.null(starts_with)) {
    if (length(starts_with) > 1) {
      tmp <- sapply(starts_with, function(x) paste0("^", x))
      zcta <- zcta[grep(paste(tmp, collapse = "|"), zcta$ZCTA5CE10), ]
    } else {
      zcta <- zcta[grep(paste0("^", starts_with), zcta$ZCTA5CE10), ]
    }
  }

  attr(zcta, "tigris") <- "zcta"

  return(zcta)

}


#' Download a Census block shapefile into R
#'
#' Description from the US Census Bureau (see link for source): Census blocks are statistical areas
#' bounded on all sides by visible features, such as streets, roads,
#' streams, and railroad tracks, and by non-visible boundaries such as city, town, township, and county
#' limits, and short line-of-sight extensions of streets and roads. Generally, census blocks are small in area;
#' for example, a block in a city. Census blocks in suburban and rural areas may be large, irregular and
#' bounded by a variety of features, such as roads, streams, and/or transmission line rights-of-way. In
#' remote areas census blocks may encompass hundreds of square miles. Census blocks cover all territory
#' in the United States, Puerto Rico, and the Island areas. Blocks do not cross the boundaries of any entity
#' for which the Census Bureau tabulates data.
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
#'        subset for, or a vector of FIPS codes if you desire multiple counties.
#'        Can also be a county name or vector of names.
#' @param ... arguments to be passed to the underlying `load_tiger` function, which is not exported.
#'        Options include \code{refresh}, which specifies whether or not to re-download shapefiles
#'        (defaults to \code{FALSE}), and \code{year}, the year for which you'd like to download data
#'        (defaults to 2014).
#' @family general area functions
#' @seealso \url{http://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2014/TGRSHP2014_TechDoc.pdf}
#' @export
#' @examples \dontrun{#'
#' # Simple example using Rose Island, American Samoa
#' # Be careful with Census blocks for states!
#'
#' library(tigris)
#'
#' rose_island <- blocks(state = "AS", county = "Rose Island")
#'
#' leaflet(rose_island) %>%
#'   addTiles() %>%
#'   addPolygons()
#'
#' }
blocks <- function(state, county = NULL, year = 2014, ...) {

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  if (year >= 2014) {
    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/TABBLOCK/tl_2014_",
                  state,
                  "_tabblock10.zip")
  } else if (year >= 2011) {
    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2014/TABBLOCK/tl_2014_",
                  state,
                  "_tabblock.zip")
  } else if (year == 2010) {
    url <- paste0("http://www2.census.gov/geo/tiger/TIGER2010/TABBLOCK/2010/tl_2014_",
                  state,
                  "_tabblock10.zip")
  } else {
    stop()
  }

  blks <- load_tiger(url, tigris_type="block", year = year, ...)

  if (!is.null(county)) {

    county <- sapply(county, function(x) validate_county(state, x))

    blks <- blks[blks$COUNTYFP10 %in% county, ]

  }

  attr(blks, "tigris") <- "block"

  return(blks)

}
