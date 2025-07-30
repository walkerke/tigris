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
counties <- function(
    state = NULL,
    cb = FALSE,
    resolution = "500k",
    year = NULL,
    ...
) {
    check_cb(cb)
    year <- set_tigris_year(year, min_year = 1990)

    if (cb) {
        resolution <- match_resolution(resolution)

        if (year %in% c(1990, 2000)) {
            suf <- year_suffix(year)

            url <- sprintf(
                "https://www2.census.gov/geo/tiger/PREVGENZ/co/co%sshp/co99_d%s_shp.zip",
                suf,
                suf
            )
        } else if (year == 2010) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_%s.zip",
                resolution
            )
        } else {
            if (year > 2013) {
                url <- sprintf(
                    "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_county_%s.zip",
                    year,
                    year,
                    resolution
                )
            } else {
                url <- sprintf(
                    "https://www2.census.gov/geo/tiger/GENZ%s/cb_%s_us_county_%s.zip",
                    year,
                    year,
                    resolution
                )
            }
        }
    } else {
        if (year == 1990) {
            cli_abort("Please specify `cb = TRUE` to get 1990 data.")
        }

        if (year %in% c(2000, 2010)) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER2010/COUNTY/%s/tl_2010_us_county%s.zip",
                year,
                year_suffix(year)
            )
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER%s/COUNTY/tl_%s_us_county.zip",
                year,
                year
            )
        }
    }

    ctys <- load_tiger(url, tigris_type = "county", ...)

    state <- validate_state(state, multiple = TRUE)

    if (!is.null(state)) {
        ctys <- ctys[ctys$STATEFP %in% state, ]
    }

    # Dissolve polygons for 1990 and 2000 CB
    if (cb && year %in% c(1990, 2000)) {
        sclass <- class(ctys)
        if (!any(sclass == "sf")) {
            ctys <- st_as_sf(ctys)
        }
        if (year == 1990) {
            ctys <- ctys %>%
                mutate(id = paste0(ST, CO)) %>%
                group_by(id) %>%
                summarize(
                    AREA = sum(AREA),
                    PERIMETER = sum(PERIMETER),
                    ST = first(ST),
                    CO = first(CO),
                    CO99_D90_ = first(CO99_D90_),
                    CO99_D90_I = first(CO99_D90_I),
                    NAME = first(NAME),
                    COUNTYFP = first(COUNTYFP),
                    STATEFP = first(STATEFP)
                ) %>%
                select(-id) %>%
                st_cast("MULTIPOLYGON")
        } else if (year == 2000) {
            ctys <- ctys %>%
                mutate(id = paste0(STATE, COUNTY)) %>%
                group_by(id) %>%
                summarize(
                    AREA = sum(AREA),
                    PERIMETER = sum(PERIMETER),
                    STATE = first(STATE),
                    COUNTY = first(COUNTY),
                    CO99_D00_ = first(CO99_D00_),
                    CO99_D00_I = first(CO99_D00_I),
                    NAME = first(NAME),
                    LSAD = first(LSAD),
                    LSAD_TRANS = first(LSAD_TRANS),
                    COUNTYFP = first(COUNTYFP),
                    STATEFP = first(STATEFP)
                ) %>%
                select(-id) %>%
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
#' @param resolution The resolution of the cartographic boundary file (if using cb = TRUE).
#'        Defaults to '500k'; the other option is '5m' (1:5 million).  Resolution of '5m' is #'        only available for the national Census tract file for years 2022 and later.
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
tracts <- function(
    state = NULL,
    county = NULL,
    cb = FALSE,
    resolution = "500k",
    year = NULL,
    ...
) {
    check_cb(cb)
    year <- set_tigris_year(year, min_year = 1990)

    if (
        resolution == "5m" && (year < 2022 || !is.null(state))
    ) {
        cli_abort(
            "`resolution = '5m'` for Census tracts is only available for the national Census tract CB file in years 2022 and later."
        )
    }

    if (is.null(state)) {
        if (year > 2018 && cb) {
            state <- "us"
            cli_inform("Retrieving Census tracts for the entire United States")
        } else {
            cli_abort(
                "A state must be specified for this year/dataset combination."
            )
        }
    } else {
        state <- validate_state(state, require_state = TRUE)
    }

    if (cb) {
        if (year %in% c(1990, 2000)) {
            suf <- year_suffix(year)

            url <- sprintf(
                "https://www2.census.gov/geo/tiger/PREVGENZ/tr/tr%sshp/tr%s_d%s_shp.zip",
                suf,
                state,
                suf
            )
        } else if (year == 2010) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_140_00_500k.zip",
                state
            )
        } else {
            if (year > 2013) {
                resolution <- match_resolution(resolution)

                url <- sprintf(
                    "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_tract_%s.zip",
                    year,
                    year,
                    state,
                    resolution
                )
            } else {
                url <- sprintf(
                    "https://www2.census.gov/geo/tiger/GENZ%s/cb_%s_%s_tract_500k.zip",
                    year,
                    year,
                    state
                )
            }
        }
    } else {
        if (year == 1990) {
            cli_abort(
                "Please specify `cb = TRUE` to get 1990 data."
            )
        }

        if (year %in% c(2000, 2010)) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER2010/TRACT/%s/tl_2010_%s_tract%s.zip",
                year,
                state,
                year_suffix(year)
            )
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER%s/TRACT/tl_%s_%s_tract.zip",
                year,
                year,
                state
            )
        }
    }

    trcts <- load_tiger(url, tigris_type = "tract", ...)

    if (!is.null(county)) {
        # TODO: Add check that state is not "us"
        county <- validate_county(state, county, multiple = TRUE)

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
                mutate(
                    TRACTSUF = ifelse(
                        is.na(TRACTSUF),
                        "00",
                        TRACTSUF
                    )
                ) %>%
                mutate(
                    id = paste0(
                        ST,
                        CO,
                        TRACTBASE,
                        TRACTSUF
                    )
                ) %>%
                group_by(id) %>%
                summarize(
                    AREA = sum(AREA),
                    PERIMETER = sum(PERIMETER),
                    ST = first(ST),
                    CO = first(CO),
                    TRACTBASE = first(TRACTBASE),
                    TRACTSUF = first(TRACTSUF),
                    TRACT_NAME = first(TRACT_NAME),
                    COUNTYFP = first(COUNTYFP),
                    STATEFP = first(STATEFP)
                ) %>%
                select(-id) %>%
                st_cast("MULTIPOLYGON")
        } else if (year == 2000) {
            trcts <- trcts %>%
                mutate(TRACT = str_pad(TRACT, 6, "right", "0")) %>%
                mutate(id = paste0(STATE, COUNTY, TRACT)) %>%
                group_by(id) %>%
                summarize(
                    AREA = sum(AREA),
                    PERIMETER = sum(PERIMETER),
                    STATE = first(STATE),
                    COUNTY = first(COUNTY),
                    TRACT = first(TRACT),
                    NAME = first(NAME),
                    LSAD = first(LSAD),
                    COUNTYFP = first(COUNTYFP),
                    STATEFP = first(STATEFP)
                ) %>%
                select(-id) %>%
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
school_districts <- function(
    state = NULL,
    type = 'unified',
    cb = FALSE,
    year = NULL,
    ...
) {
    check_cb(cb)
    year <- set_tigris_year(year)
    type <- arg_match(type, c("unified", "elementary", "secondary"))

    if (is.null(state)) {
        if (year > 2018 && cb) {
            state <- "us"
            cli_inform(
                "Retrieving school districts for the entire United States"
            )
        } else {
            cli_abort(
                "A state must be specified for this year/dataset combination."
            )
        }
    } else {
        state <- validate_state(state, require_state = TRUE)
    }

    type <- switch(
        type,
        unified = "unsd",
        elementary = "elsd",
        secondary = "scsd"
    )

    if (cb) {
        url <- sprintf(
            "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_%s_500k.zip",
            year,
            year,
            state,
            type
        )
    } else {
        url <- sprintf(
            "https://www2.census.gov/geo/tiger/TIGER%s/%s/tl_%s_%s_%s.zip",
            year,
            toupper(type),
            year,
            state,
            type
        )
    }

    load_tiger(url, tigris_type = type, ...)
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
block_groups <- function(
    state = NULL,
    county = NULL,
    cb = FALSE,
    year = NULL,
    ...
) {
    check_cb(cb)
    year <- set_tigris_year(year, min_year = 1990)

    if (is.null(state)) {
        if (year > 2018 && cb) {
            state <- "us"
            cli_inform(
                "Retrieving Census block groups for the entire United States"
            )
        } else {
            cli_abort(
                "A state must be specified for this year/dataset combination."
            )
        }
    } else {
        state <- validate_state(state, require_state = TRUE)
    }

    if (cb) {
        if (year %in% c(1990, 2000)) {
            suf <- year_suffix(year)

            url <- sprintf(
                "https://www2.census.gov/geo/tiger/PREVGENZ/bg/bg%sshp/bg%s_d%s_shp.zip",
                suf,
                state,
                suf
            )
        } else if (year == 2010) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_150_00_500k.zip",
                state
            )
        } else {
            if (year > 2013) {
                url <- sprintf(
                    "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_bg_500k.zip",
                    year,
                    year,
                    state
                )
            } else {
                url <- sprintf(
                    "https://www2.census.gov/geo/tiger/GENZ%s/cb_%s_%s_bg_500k.zip",
                    year,
                    year,
                    state
                )
            }
        }
    } else {
        if (year == 1990) {
            cli_abort(
                "Please specify `cb = TRUE` to get 1990 data."
            )
        }

        if (year %in% c(2000, 2010)) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER2010/BG/%s/tl_2010_%s_bg%s.zip",
                year,
                state,
                year_suffix(year)
            )
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER%s/BG/tl_%s_%s_bg.zip",
                year,
                year,
                state
            )
        }
    }

    bgs <- load_tiger(url, tigris_type = "block_group", ...)

    if (!is.null(county)) {
        county <- validate_county(state, county, multiple = TRUE)

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
                group_by(GEOID) %>%
                summarize(
                    AREA = sum(AREA),
                    PERIMETER = sum(PERIMETER),
                    ST = first(ST),
                    CO = first(CO),
                    TRACT = first(TRACT),
                    BG = first(BG),
                    AREALAND = first(AREALAND),
                    AREAWAT = first(AREAWAT),
                    AREATOT = first(AREATOT),
                    NAME = first(NAME),
                    COUNTYFP = first(COUNTYFP),
                    STATEFP = first(STATEFP)
                ) %>%
                st_cast("MULTIPOLYGON")
        } else if (year == 2000) {
            bgs <- bgs %>%
                mutate(TRACT = str_pad(TRACT, 6, "right", "0")) %>%
                mutate(
                    id = paste0(
                        STATE,
                        COUNTY,
                        TRACT,
                        BLKGROUP
                    )
                ) %>%
                group_by(id) %>%
                summarize(
                    AREA = sum(AREA),
                    PERIMETER = sum(PERIMETER),
                    STATE = first(STATE),
                    COUNTY = first(COUNTY),
                    TRACT = first(TRACT),
                    BLKGROUP = first(BLKGROUP),
                    NAME = first(NAME),
                    LSAD = first(LSAD),
                    LSAD_TRANS = first(LSAD_TRANS),
                    COUNTYFP = first(COUNTYFP),
                    STATEFP = first(STATEFP)
                ) %>%
                select(-id) %>%
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
zctas <- function(
    cb = FALSE,
    starts_with = NULL,
    year = NULL,
    state = NULL,
    ...
) {
    check_cb(cb)
    year <- set_tigris_year(
        year,
        message =  "Zip Code Tabulation Areas (ZCTAs) are only available beginning with the 2000 Census.",
        min_year = 2000
    )

    if (year > 2020 && cb) {
        cli_abort(
            c(
                "The Census Bureau has not yet released the CB ZCTA file for {year}.",
                "*" = "Please set `year = 2020` or `cb = FALSE` instead."
            )
        )
    }

    if (!is.null(state) && year > 2010) {
        cli_abort("ZCTAs are only available by state for 2000 and 2010, not {year}.")
    }

    if (!is.null(state) && year == 2010 && cb) {
        cli_abort(
            "ZCTAs are only available by state for 2010 when `cb = FALSE`."
        )
    }

    state <- validate_state(state)

    cache <- getOption("tigris_use_cache")

    if (is.null(cache)) {
        cli_inform(
            c(
                "!" = "ZCTAs can take several minutes to download.",
                "*" = "Set `options(tigris_use_cache = TRUE)` to cache the data and avoid re-downloading in future R sessions."
            )
        )
    }

    if (cb) {
        if (year == 2000) {
            if (is.null(state)) {
                url <- "https://www2.census.gov/geo/tiger/PREVGENZ/zt/z500shp/zt99_d00_shp.zip"
            } else {
                url <- sprintf(
                    "https://www2.census.gov/geo/tiger/PREVGENZ/zt/z500shp/zt%s_d00_shp.zip",
                    state
                )
            }
        } else if (year == 2010) {
            url <- "https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_860_00_500k.zip"
        } else if (year >= 2020) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_zcta520_500k.zip",
                year,
                year
            )
        } else if (year < 2020) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_zcta510_500k.zip",
                year,
                year
            )

            if (year == 2013) url <- gsub("shp/", "", url)
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_zcta520_500k.zip",
                year,
                year
            )
        }
    } else {
        if (year >= 2020) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER%s/ZCTA520/tl_%s_us_zcta520.zip",
                year,
                year
            )
        } else {
            if (year %in% c(2000, 2010)) {
                suf <- year_suffix(year)

                if (is.null(state)) {
                    url <- sprintf(
                        "https://www2.census.gov/geo/tiger/TIGER2010/ZCTA5/%s/tl_2010_us_zcta5%s.zip",
                        year,
                        suf
                    )
                } else {
                    url <- sprintf(
                        "https://www2.census.gov/geo/tiger/TIGER2010/ZCTA5/%s/tl_2010_%s_zcta5%s.zip",
                        year,
                        state,
                        suf
                    )
                }
            } else {
                url <- sprintf(
                    "https://www2.census.gov/geo/tiger/TIGER%s/ZCTA5/tl_%s_us_zcta510.zip",
                    year,
                    year
                )
            }
        }
    }

    zcta <- load_tiger(url, tigris_type = "zcta", ...)

    # Handle split ZCTAs in 2000 CB file
    if (year == 2000 && cb) {
        cli_inform(
            c(
               "!" = "CB ZCTAs for 2000 include separate polygons for discontiguous parts.",
                "*" = "Combine by summarizing over the ZCTA column but note that this can be a time-consuming operation."
            )
        )
    }

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
    year <- set_tigris_year(
        year,
        min_year = 2000,
        not_year = c(2001:2009)
    )

    state <- validate_state(state, require_state = TRUE)

    if (year >= 2014) {
        if (year >= 2020) {
            # New block logic for 2020
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER%s/TABBLOCK20/tl_%s_%s_tabblock20.zip",
                year,
                year,
                state
            )
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER%s/TABBLOCK/tl_%s_%s_tabblock10.zip",
                year,
                year,
                state
            )
        }
    } else if (year %in% 2011:2013) {
        url <- sprintf(
            "https://www2.census.gov/geo/tiger/TIGER%s/TABBLOCK/tl_%s_%s_tabblock.zip",
            year,
            year,
            state
        )
    } else if (year %in% c(2000, 2010)) {
        county <- validate_county(state, county, multiple = TRUE, allow_null = TRUE)
        suf <- year_suffix(year)

        if (!is.null(county)) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER2010/TABBLOCK/%s/tl_2010_%s%s_tabblock%s.zip",
                year,
                state,
                county,
                suf
            )
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER2010/TABBLOCK/%s/tl_2010_%s_tabblock%s.zip",
                year,
                state,
                suf
            )
        }
    }


    if (length(county) > 1 && year < 2011) {
        blks <- lapply(url, function(x) {
            load_tiger(x, tigris_type = "block", ...)
        }) %>%
            rbind_tigris()
    } else {
        blks <- load_tiger(url, tigris_type = "block", ...)
    }

    if (!is.null(county) && year > 2010) {
        county <- validate_county(state, county, multiple = TRUE)

        if (year >= 2020) {
            blks <- blks[blks$COUNTYFP20 %in% county, ]
        } else {
            blks <- blks[blks$COUNTYFP10 %in% county, ]
        }
    }

    attr(blks, "tigris") <- "block"

    blks
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
county_subdivisions <- function(
    state,
    county = NULL,
    cb = FALSE,
    year = NULL,
    ...
) {
    check_cb(cb)
    year <- set_tigris_year(year, min_year = 2010)
    state <- validate_state(state, require_state = TRUE)

    if (cb) {
        if (year == 2010) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_060_00_500k.zip",
                state
            )
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_cousub_500k.zip",
                year,
                year,
                state
            )

            if (year == 2013) url <- gsub("shp/", "", url)
        }
    } else {
        if (year == 2010) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER2010/COUSUB/2010/tl_2010_%s_cousub10.zip",
                state
            )
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER%s/COUSUB/tl_%s_%s_cousub.zip",
                year,
                year,
                state
            )
        }
    }

    cs <- load_tiger(url, tigris_type = "county_subdivision", ...)

    if (!is.null(county)) {
        county <- validate_county(state, county, multiple = TRUE)
        cs <- cs[cs$COUNTYFP %in% county, ]
    }

    attr(cs, "tigris") <- "county_subdivision"
    cs
}
