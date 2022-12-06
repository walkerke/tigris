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
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#'
#' @export
#' @family general area functions
#' @seealso \url{https://www2.census.gov/geo/pdfs/reference/GARM/Ch4GARM.pdf}
#' @examples \dontrun{
#' library(tigris)
#' library(ggplot2)
#'
#' me <- counties("Maine", cb = TRUE)
#'
#' gg <- ggplot()
#' gg <- gg + geom_sf(data = me, color="black",
#'                    fill="white", size=0.25)
#' gg
#' }
counties <- function(state = NULL, cb = FALSE, resolution = '500k', year = NULL, ...) {

  if (!(resolution %in% c('500k', '5m', '20m'))) {
    stop("Invalid value for resolution. Valid values are '500k', '5m', and '20m'.", call. = FALSE)
  }

  if (is.null(year)) {

    year <- getOption("tigris_year", 2021)

    message(sprintf("Retrieving data for the year %s", year))

  }

    cyear <- as.character(year)

  if (cb == TRUE) {

    if (year %in% c(1990, 2000)) {

      suf <- substr(as.character(year), 3, 4)

      url <- sprintf("https://www2.census.gov/geo/tiger/PREVGENZ/co/co%sshp/co99_d%s_shp.zip",
                     suf, suf)

    } else if (year == 2010) {

      url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_%s.zip",
                     resolution)

    } else {

      if (year > 2013) {

        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_county_%s.zip",
                       cyear, cyear, resolution)

      } else {

        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/cb_%s_us_county_%s.zip",
                       cyear, cyear, resolution)

      }


    }

  } else {

    if (year == 1990) stop("Please specify `cb = TRUE` to get 1990 data.", call. = FALSE)

    if (year %in% c(2000, 2010)) {

      suf <- substr(cyear, 3, 4)

      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/COUNTY/%s/tl_2010_us_county%s.zip",
                     cyear, suf)

    } else {

      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/COUNTY/tl_%s_us_county.zip",
                     cyear, cyear)

    }

  }

  ctys <- load_tiger(url, tigris_type="county", ...)

  state <- unlist(sapply(state, validate_state, USE.NAMES=FALSE))

  if (!is.null(state)) {

    ctys <- ctys[ctys$STATEFP %in% state,]

  }

  # Dissolve polygons for 1990 and 2000 CB
  if (cb && year %in% c(1990, 2000)) {
    sclass <- class(ctys)
    if (!any(sclass == "sf")) {
      ctys <- st_as_sf(ctys)
    }
    if (year == 1990) {
      ctys <- ctys %>%
        mutate(id = paste0(.data$ST, .data$CO)) %>%
        group_by(.data$id) %>%
        summarize(AREA = sum(.data$AREA),
                  PERIMETER = sum(.data$PERIMETER),
                  ST = first(.data$ST),
                  CO = first(.data$CO),
                  CO99_D90_ = first(.data$CO99_D90_),
                  CO99_D90_I = first(.data$CO99_D90_I),
                  NAME = first(.data$NAME),
                  COUNTYFP = first(.data$COUNTYFP),
                  STATEFP = first(.data$STATEFP)) %>%
        select(-.data$id) %>%
        st_cast("MULTIPOLYGON")

    } else if (year == 2000) {
      ctys <- ctys %>%
        mutate(id = paste0(.data$STATE, .data$COUNTY)) %>%
        group_by(.data$id) %>%
        summarize(AREA = sum(.data$AREA),
                  PERIMETER = sum(.data$PERIMETER),
                  STATE = first(.data$STATE),
                  COUNTY = first(.data$COUNTY),
                  CO99_D00_ = first(.data$CO99_D00_),
                  CO99_D00_I = first(.data$CO99_D00_I),
                  NAME = first(.data$NAME),
                  LSAD = first(.data$LSAD),
                  LSAD_TRANS = first(.data$LSAD_TRANS),
                  COUNTYFP = first(.data$COUNTYFP),
                  STATEFP = first(.data$STATEFP)) %>%
        select(-.data$id) %>%
        st_cast("MULTIPOLYGON")
    }
    if (any(sclass == "SpatialPolygonsDataFrame")) {
      ctys <- as(ctys, "Spatial")
    }
  }

  attr(ctys, 'tigris') <- 'county'

  return(ctys)

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
#' depending on the density of settlement.  Census tract boundaries are
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
#'        be state name or state abbreviation. When \code{NULL} and combined with
#'        \code{cb = TRUE}, a national dataset of Census tracts will be returned for
#'        years 2019 and later.
#' @param county The three-digit FIPS code (string) of the county you'd like to
#'        subset for, or a vector of FIPS codes if you desire multiple counties.
#'        Can also be a county name or vector of names.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        tracts file.  Defaults to FALSE (the most detailed TIGER/Line file)
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family general area functions
#' @seealso \url{https://www2.census.gov/geo/pdfs/reference/GARM/Ch10GARM.pdf}
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
tracts <- function(state = NULL, county = NULL, cb = FALSE, year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2021)

    message(sprintf("Retrieving data for the year %s", year))

  }

  if (is.null(state)) {
    if (year > 2018 && cb == TRUE) {
      state <- "us"
      message("Retrieving Census tracts for the entire United States")
    } else {
      stop("A state must be specified for this year/dataset combination.",
           call. = FALSE)
    }
  } else {
    state <- validate_state(state)

    if (is.null(state)) stop("Invalid state", call.=FALSE)
  }

  if (cb == TRUE) {

    if (year %in% c(1990, 2000)) {

      suf <- substr(as.character(year), 3, 4)

      url <- sprintf("https://www2.census.gov/geo/tiger/PREVGENZ/tr/tr%sshp/tr%s_d%s_shp.zip",
                     suf, state, suf)

    } else if (year == 2010) {

      url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_140_00_500k.zip",
                     state)

    } else {

      if (year > 2013) {

        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_tract_500k.zip",
                       as.character(year), as.character(year), state)

      } else {

        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/cb_%s_%s_tract_500k.zip",
                       as.character(year), as.character(year), state)

      }


    }

  } else {

    if (year == 1990) stop("Please specify `cb = TRUE` to get 1990 data.", call. = FALSE)

    if (year %in% c(2000, 2010)) {

      cyear <- as.character(year)

      suf <- substr(cyear, 3, 4)

      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/TRACT/%s/tl_2010_%s_tract%s.zip",
                     cyear, state, suf)

    } else {

      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/TRACT/tl_%s_%s_tract.zip",
                     as.character(year), as.character(year), state)

    }

  }

  trcts <- load_tiger(url, tigris_type="tract", ...)

  if (!is.null(county)) {

     county <- sapply(county, function(x) { validate_county(state, x) })

     trcts <- trcts[trcts$COUNTYFP %in% county, ]

  }


# Dissolve polygons for 1990 and 2000 CB
  if (cb && year %in% c(1990, 2000)) {
    sclass <- class(trcts)
    if (!any(sclass == "sf")) {
      trcts <- st_as_sf(trcts)
    }
    if (year == 1990) {
      trcts <- trcts %>%
        mutate(TRACTSUF = ifelse(is.na(.data$TRACTSUF), "00", .data$TRACTSUF)) %>%
        mutate(id = paste0(.data$ST, .data$CO, .data$TRACTBASE, .data$TRACTSUF)) %>%
        group_by(.data$id) %>%
        summarize(AREA = sum(.data$AREA),
                  PERIMETER = sum(.data$PERIMETER),
                  ST = first(.data$ST),
                  CO = first(.data$CO),
                  TRACTBASE = first(.data$TRACTBASE),
                  TRACTSUF = first(.data$TRACTSUF),
                  TRACT_NAME = first(.data$TRACT_NAME),
                  COUNTYFP = first(.data$COUNTYFP),
                  STATEFP = first(.data$STATEFP)) %>%
        select(-.data$id) %>%
        st_cast("MULTIPOLYGON")

    } else if (year == 2000) {
      trcts <- trcts %>%
        mutate(TRACT = str_pad(.data$TRACT, 6, "right", "0")) %>%
        mutate(id = paste0(.data$STATE, .data$COUNTY, .data$TRACT)) %>%
        group_by(.data$id) %>%
        summarize(AREA = sum(.data$AREA),
                  PERIMETER = sum(.data$PERIMETER),
                  STATE = first(.data$STATE),
                  COUNTY = first(.data$COUNTY),
                  TRACT = first(.data$TRACT),
                  NAME = first(.data$NAME),
                  LSAD = first(.data$LSAD),
                  COUNTYFP = first(.data$COUNTYFP),
                  STATEFP = first(.data$STATEFP)) %>%
        select(-.data$id) %>%
        st_cast("MULTIPOLYGON")
    }
    if (any(sclass == "SpatialPolygonsDataFrame")) {
      trcts <- as(trcts, "Spatial")
    }
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
#'        be state name or state abbreviation. When \code{NULL} and combined with
#'        \code{cb = TRUE}, a national dataset of school districts will be returned for
#'        years 2019 and later.
#' @param type Specify whether you want to return a unified school district (the default, \code{'unified'}),
#'        an elementary school district (\code{'elementary'}), or a secondary school district (\code{'secondary'}).
#'        Please note: elementary and secondary school districts do not exist in all states
#' @param cb if TRUE, download a generalized (1:500k)
#'        school districts file.  Defaults to FALSE (the most detailed TIGER/Line file)
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family general area functions
#' @seealso \url{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf}
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
school_districts <- function(state = NULL, type = 'unified',
                             cb = FALSE, year = NULL, ...) {

  if (is.null(year)) {

    year = getOption("tigris_year", 2021)

    message(sprintf("Retrieving data for the year %s", year))

  }

  if (is.null(state)) {
    if (year > 2018 && cb == TRUE) {
      state <- "us"
      message("Retrieving school districts for the entire United States")
    } else {
      stop("A state must be specified for this year/dataset combination.",
           call. = FALSE)
    }
  } else {
    state <- validate_state(state)

    if (is.null(state)) stop("Invalid state", call.=FALSE)
  }

  if (year < 2011) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.", fname)

    stop(msg, call. = FALSE)

  }

  if (type == 'unified') {
    type <- 'unsd'
  } else if (type == 'elementary') {
    type <- 'elsd'
  } else if (type == 'secondary') {
    type <- 'scsd'
  } else {
    stop("Invalid school district type.  Valid types are 'unified', 'elementary', and 'secondary'.", call. = FALSE)
  }

  cyear <- as.character(year)

  if (cb) {

    url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_%s_500k.zip",
                   cyear, cyear, state, type)

  } else {

    url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/%s/tl_%s_%s_%s.zip",
                   cyear, toupper(type), cyear, state, type)

  }

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
#'        be state name or state abbreviation.  When \code{NULL} and combined with
#'        \code{cb = TRUE}, a national dataset of block groups will be returned for
#'        years 2019 and later.
#' @param county The three-digit FIPS code (string) of the county you'd like to
#'        subset for, or a vector of FIPS codes if you desire multiple counties.
#'        Can also be a county name or vector of names.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        file.  Defaults to FALSE (the most detailed TIGER/Line file)
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family general area functions
#' @seealso \url{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#'
#' benton_bgs <- block_groups("Oregon", "Benton")
#'
#' plot(benton_bgs$geometry)
#' }
block_groups <- function(state = NULL, county = NULL, cb = FALSE, year = NULL, ...) {

  if (is.null(year)) {

    year = getOption("tigris_year", 2021)

    message(sprintf("Retrieving data for the year %s", year))

  }

    cyear <- as.character(year)

  if (is.null(state)) {
    if (year > 2018 && cb == TRUE) {
      state <- "us"
      message("Retrieving Census block groups for the entire United States")
    } else {
      stop("A state must be specified for this year/dataset combination.",
           call. = FALSE)
    }
  } else {
    state <- validate_state(state)

    if (is.null(state)) stop("Invalid state", call.=FALSE)
  }

  if (cb == TRUE) {

    if (year %in% c(1990, 2000)) {

      suf <- substr(as.character(year), 3, 4)

      url <- sprintf("https://www2.census.gov/geo/tiger/PREVGENZ/bg/bg%sshp/bg%s_d%s_shp.zip",
                     suf, state, suf)

    } else if (year == 2010) {

      url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_150_00_500k.zip",
                     state)

    } else {

      if (year > 2013) {

        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_bg_500k.zip",
                       cyear, cyear, state)

      } else {

        url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/cb_%s_%s_bg_500k.zip",
                       cyear, cyear, state)

      }


    }

  } else {

    if (year == 1990) stop("Please specify `cb = TRUE` to get 1990 data.", call. = FALSE)

    if (year %in% c(2000, 2010)) {

      suf <- substr(cyear, 3, 4)

      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/BG/%s/tl_2010_%s_bg%s.zip",
                     cyear, state, suf)

    } else {

      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/BG/tl_%s_%s_bg.zip",
                     cyear, cyear, state)

    }

  }


  bgs <- load_tiger(url, tigris_type="block_group", ...)

  if (!is.null(county)) {

    county <- sapply(county, function(x) { validate_county(state, x) })

    bgs <- bgs[bgs$COUNTYFP %in% county, ]

  }

  # Dissolve polygons for 1990 and 2000 CB
  if (cb && year %in% c(1990, 2000)) {
    sclass <- class(bgs)
    if (!any(sclass == "sf")) {
      bgs <- st_as_sf(bgs)
    }
    if (year == 1990) {
      bgs <- bgs %>%
        group_by(.data$GEOID) %>%
        summarize(AREA = sum(.data$AREA),
                  PERIMETER = sum(.data$PERIMETER),
                  ST = first(.data$ST),
                  CO = first(.data$CO),
                  TRACT = first(.data$TRACT),
                  BG = first(.data$BG),
                  AREALAND = first(.data$AREALAND),
                  AREAWAT = first(.data$AREAWAT),
                  AREATOT = first(.data$AREATOT),
                  NAME = first(.data$NAME),
                  COUNTYFP = first(.data$COUNTYFP),
                  STATEFP = first(.data$STATEFP)) %>%
        st_cast("MULTIPOLYGON")
    } else if (year == 2000) {
      bgs <- bgs %>%
        mutate(TRACT = str_pad(.data$TRACT, 6, "right", "0")) %>%
        mutate(id = paste0(.data$STATE, .data$COUNTY, .data$TRACT, .data$BLKGROUP)) %>%
        group_by(.data$id) %>%
        summarize(AREA = sum(.data$AREA),
                  PERIMETER = sum(.data$PERIMETER),
                  STATE = first(.data$STATE),
                  COUNTY = first(.data$COUNTY),
                  TRACT = first(.data$TRACT),
                  BLKGROUP = first(.data$BLKGROUP),
                  NAME = first(.data$NAME),
                  LSAD = first(.data$LSAD),
                  LSAD_TRANS = first(.data$LSAD_TRANS),
                  COUNTYFP = first(.data$COUNTYFP),
                  STATEFP = first(.data$STATEFP)) %>%
        select(-.data$id) %>%
        st_cast("MULTIPOLYGON")
    }
    if (any(sclass == "SpatialPolygonsDataFrame")) {
      bgs <- as(bgs, "Spatial")
    }
  }

  attr(bgs, "tigris") <- "block_group"

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
#' @param state the state for which you are requesting data; only available for 2000 (TIGER/Line
#'              and CB shapefiles) and 2010 (TIGER/Line shapefiles only)
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family general area functions
#' @seealso \url{https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html}
#' @export
#' @examples \dontrun{
#' # Example: get ZCTAs that intersect the Memphis, TN urbanized area
#'
#' library(tigris)
#' zcta1 <- zctas(cb = TRUE, starts_with = c("37", "38", "72"))
#'
#' uas <- urban_areas()
#'
#' memphis_ua <- uas[grep("Memphis", uas$NAME10), ]
#'
#' mem_zcta <- zcta1[memphis_ua, ]
#'
#' plot(mem_zcta$geometry)
#'
#' }
zctas <- function(cb = FALSE, starts_with = NULL, year = NULL, state = NULL, ...) {

  if (is.null(year)) {

    year = getOption("tigris_year", 2021)

    message(sprintf("Retrieving data for the year %s", year))

  }

  if (!is.null(state) && year > 2010) {
    stop("ZCTAs are only available by state for 2000 and 2010.")
  }

  if (!is.null(state) && year == 2010 && cb == TRUE) {
    stop("ZCTAs are only available by state for 2010 when cb = FALSE.", call. = FALSE)
  }

  if (year == 1990) {
    stop("Zip Code Tabulation Areas are only available beginning with the 2000 Census.",
         call. = FALSE)
  }

  if (!is.null(state)) state <- validate_state(state)

  cache <- getOption("tigris_use_cache")

  if (is.null(cache)) {
    message("ZCTAs can take several minutes to download.  To cache the data and avoid re-downloading in future R sessions, set `options(tigris_use_cache = TRUE)`")
  }

  cyear <- as.character(year)

  if (cb) {

    if (year == 2000) {
      if (is.null(state)) {
        url <- "https://www2.census.gov/geo/tiger/PREVGENZ/zt/z500shp/zt99_d00_shp.zip"
      } else {
        url <- sprintf("https://www2.census.gov/geo/tiger/PREVGENZ/zt/z500shp/zt%s_d00_shp.zip",
                       state)
      }
    } else if (year == 2010) {

      url <- "https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_860_00_500k.zip"
    } else if (year >= 2020) {
      url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_zcta520_500k.zip",
                     cyear, cyear)
    } else if (year < 2020) {
      url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_zcta510_500k.zip",
                     cyear, cyear)

      if (year == 2013) url <- gsub("shp/", "", url)
    } else {
      url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_zcta520_500k.zip",
                     cyear, cyear)
    }

  } else {

    if (year >= 2020) {
      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/ZCTA520/tl_%s_us_zcta520.zip",
                     cyear, cyear)
    } else {

      if (year %in% c(2000, 2010)) {

        suf <- substr(cyear, 3, 4)

        if (is.null(state)) {
          url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/ZCTA5/%s/tl_2010_us_zcta5%s.zip",
                         cyear, suf)
        } else {
          url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/ZCTA5/%s/tl_2010_%s_zcta5%s.zip",
                         cyear, state, suf)
        }
      } else {
        url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/ZCTA5/tl_%s_us_zcta510.zip",
                       cyear, cyear)
      }

    }

  }

  zcta <- load_tiger(url, tigris_type="zcta", ...)

  if (!is.null(starts_with)) {
    nms <- names(zcta)
    col <- grep("ZCTA", nms)
    if (length(starts_with) > 1) {
      tmp <- sapply(starts_with, function(x) paste0("^", x))
      zcta <- zcta[grep(paste(tmp, collapse = "|"), zcta[[col]]), ]
    } else {
      zcta <- zcta[grep(paste0("^", starts_with), zcta[[col]]), ]
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
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family general area functions
#' @seealso \url{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' # Simple example using Rose Island, American Samoa
#' # Be careful with Census blocks for states!
#'
#' library(tigris)
#' library(leaflet)
#'
#' rose_island <- blocks(state = "AS", county = "Rose Island")
#'
#' leaflet(rose_island) %>%
#'   addTiles() %>%
#'   addPolygons()
#'
#' }
blocks <- function(state, county = NULL, year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2021)

    message(sprintf("Retrieving data for the year %s", year))


  }

  if (length(county) > 1 && year < 2011) {
    p <- lapply(county, function(x) {
      blocks(state = state, county = x, year = year, ...)
    }) %>%
      rbind_tigris()

    return(p)
  }

  if (year < 2000) {

    fname <- as.character(match.call())[[1]]

    msg <- "Block data are not available for 1990."

    stop(msg, call. = FALSE)

  }

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  cyear <- as.character(year)

  if (year >= 2014) {

    if (year %in% 2020:2021) {

      # New block logic for 2020
      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/TABBLOCK20/tl_%s_%s_tabblock20.zip",
                     cyear, cyear, state)

    } else {

      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/TABBLOCK/tl_%s_%s_tabblock10.zip",
                     cyear, cyear, state)

    }

  } else if (year %in% 2011:2013) {
    url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/TABBLOCK/tl_%s_%s_tabblock.zip",
                   cyear, cyear, state)
  } else if (year %in% c(2000, 2010)) {
    suf <- substr(cyear, 3, 4)

    if (!is.null(county)) {

      county <- validate_county(state, county)

      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/TABBLOCK/%s/tl_2010_%s%s_tabblock%s.zip",
                     cyear, state, county, suf)
    } else {

      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/TABBLOCK/%s/tl_2010_%s_tabblock%s.zip",
                     cyear, state, suf)
    }

  } else {
    stop()
  }

  blks <- load_tiger(url, tigris_type="block", ...)

  if (!is.null(county) && year > 2010) {

    if (year >= 2020) {
      county <- sapply(county, function(x) { validate_county(state, x) })

      blks <- blks[blks$COUNTYFP20 %in% county, ]
    } else {

      county <- sapply(county, function(x) { validate_county(state, x) })

      blks <- blks[blks$COUNTYFP10 %in% county, ]

    }

  }

  attr(blks, "tigris") <- "block"

  return(blks)

}

#' Download a county subdivision shapefile into R
#'
#' From the US Census Bureau (see link for source, and more information): "All counties and
#' statistically equivalent entities consist of one or more geographic units that the Bureau
#' of the Census recognizes as county subdivisions. The two major types of county subdivisions
#'  are minor civil divisions(MCDs) and census county divisions (CCDs).
#'  A State has either MCDs or their statistical equivalents, or CCDs; it cannot
#'  contain both."
#'
#' @param state The two-digit FIPS code (string) of the state you want. Can also
#'        be state name or state abbreviation.
#' @param county The three-digit FIPS code (string) of the county you'd like to
#'        subset for, or a vector of FIPS codes if you desire multiple counties.
#'        Can also be a county name or vector of names.
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        file.  Defaults to FALSE (the most detailed TIGER/Line file)
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family general area functions
#' @seealso \url{https://www2.census.gov/geo/pdfs/reference/GARM/Ch8GARM.pdf}
#' @export
#'
#' @examples \dontrun{
#' library(tigris)
#'
#' or <- county_subdivisions('Oregon', c('Linn', 'Benton'))
#'
#' plot(or$geometry)
#'
#' }
county_subdivisions <- function(state, county = NULL, cb = FALSE, year = NULL, ...) {

  if (is.null(year)) {

    year <- getOption("tigris_year", 2021)

    message(sprintf("Retrieving data for the year %s", year))


  }

  if (year < 2010) {

    fname <- as.character(match.call())[[1]]

    msg <- sprintf("%s is not currently available for years prior to 2010.", fname)

    stop(msg, call. = FALSE)

  }

  state <- validate_state(state)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  cyear <- as.character(year)

  if (cb == TRUE) {

    if (year == 2010) {
      url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_060_00_500k.zip",
                     state)
    } else {

      url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_cousub_500k.zip",
                     cyear, cyear, state)

      if (year == 2013) url <- gsub("shp/", "", url)

    }

  } else {

    if (year == 2010) {
      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/COUSUB/2010/tl_2010_%s_cousub10.zip", state)
    } else {
      url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/COUSUB/tl_%s_%s_cousub.zip",
                     cyear, cyear, state)
    }


  }

  cs <- load_tiger(url, tigris_type="county_subdivision", ...)

  if (!is.null(county)) {

    county <- sapply(county, function(x) { validate_county(state, x) })

    cs <- cs[cs$COUNTYFP %in% county, ]

  }

  attr(cs, "tigris") <- "county_subdivision"

  return(cs)

}
