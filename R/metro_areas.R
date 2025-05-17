#' Download a core-based statistical area shapefile into R
#'
#' Core-based statistical areas include both metropolitan areas and micropolitan areas.  The US Census
#' Bureau defines these areas as follows: "A metro area contains a core urban area of 50,000 or more population, and a
#' micro area contains an urban core of at least 10,000 (but less than 50,000) population. Each metro or micro area
#' consists of one or more counties and includes the counties containing the core urban area, as well as any adjacent
#' counties that have a high degree of social and economic integration (as measured by commuting to work) with the urban
#' core."  Please see the link provided for more information
#'
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @param resolution The resolution of the cartographic boundary file (if cb == TRUE).
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family metro area functions
#' @seealso \url{https://www.census.gov/programs-surveys/metro-micro.html}
#' @export
core_based_statistical_areas <- function(
    cb = FALSE,
    resolution = '500k',
    year = NULL,
    ...
) {
    if (is.null(year)) {
        year <- getOption("tigris_year", 2024)

        message(sprintf("Retrieving data for the year %s", year))
    }

    if (year < 2010) {
        fname <- as.character(match.call())[[1]]

        msg <- sprintf(
            "%s is not currently available for years prior to 2010.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.",
            fname
        )

        stop(msg, call. = FALSE)
    }

    if (!(resolution %in% c('500k', '5m', '20m'))) {
        stop(
            "Invalid value for resolution. Valid values are '500k', '5m', and '20m'.",
            call. = FALSE
        )
    }

    cyear <- as.character(year)

    if (cb == TRUE) {
        if (year == 2010) {
            if (resolution == "5m")
                stop(
                    "Available resolutions are '500k' and '20m'",
                    call. = FALSE
                )
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_310_m1_%s.zip",
                resolution
            )
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_cbsa_%s.zip",
                cyear,
                cyear,
                resolution
            )

            if (year == 2013) url <- gsub("shp/", "", url)
        }
    } else {
        if (year == 2010) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER2010/CBSA/2010/tl_2010_us_cbsa10.zip"
            )
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER%s/CBSA/tl_%s_us_cbsa.zip",
                cyear,
                cyear
            )
        }
    }

    return(load_tiger(url, tigris_type = "cbsa", ...))
}

#' Download an urban areas shapefile into R
#'
#' Urban areas include both "urbanized areas," which are densely developed areas with a population of at least 50,000,
#'  and "urban clusters," which have a population of greater than 2,500 but less than 50,000.  For more information,
#' please see the link provided.
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @param criteria If set to "2020" and the year is 2020, will download the new 2020 urban areas criteria. Not available for cartographic boundary shapefiles / other years at the moment.
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family metro area functions
#' @seealso \url{https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural.html}
#' @export
urban_areas <- function(cb = FALSE, year = NULL, criteria = NULL, ...) {
    if (is.null(year)) {
        year <- getOption("tigris_year", 2024)

        message(sprintf("Retrieving data for the year %s", year))
    }

    if (year < 2011) {
        fname <- as.character(match.call())[[1]]

        msg <- sprintf(
            "%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.",
            fname
        )

        stop(msg, call. = FALSE)
    }

    cyear <- as.character(year)

    if (cb) {
        if (!is.null(criteria)) {
            stop(
                "The `criteria` argument is not supported for cartographic boundary files",
                call. = FALSE
            )
        }

        url <- sprintf(
            "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_ua10_500k.zip",
            cyear,
            cyear
        )

        if (year == 2013) url <- gsub("shp/", "", url)
    } else {
        if (year >= 2023) {
            if (year == 2023) {
                url <- sprintf(
                    "https://www2.census.gov/geo/tiger/TIGER%s/UAC/tl_%s_us_uac20.zip",
                    cyear,
                    cyear
                )
            } else {
                url <- sprintf(
                    "https://www2.census.gov/geo/tiger/TIGER%s/UAC20/tl_%s_us_uac20.zip",
                    cyear,
                    cyear
                )
            }
        } else if (!is.null(criteria) && criteria == "2020") {
            if (year != 2020) {
                stop(
                    "2020 criteria is only supported when `year` is set to 2020 at the moment.",
                    call. = FALSE
                )
            }

            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER%s/UAC/tl_%s_us_uac20.zip",
                cyear,
                cyear
            )
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER%s/UAC/tl_%s_us_uac10.zip",
                cyear,
                cyear
            )
        }
    }

    return(load_tiger(url, tigris_type = "urban", ...))
}

#' Download a combined statistical areas shapefile into R
#'
#' Combined statistical areas are "two or more adjacent CBSAs that have significant
#' employment interchanges."  In turn, CSAs are composed of multiple metropolitan and/or micropolitan areas, and should
#' not be compared with individual core-based statistical areas.
#'
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).
#' @param resolution The resolution of the cartographic boundary file (if cb == TRUE).
#'        Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family metro area functions
#' @seealso \url{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf}
#' @export
combined_statistical_areas <- function(
    cb = FALSE,
    resolution = '500k',
    year = NULL,
    ...
) {
    if (is.null(year)) {
        year <- getOption("tigris_year", 2024)

        message(sprintf("Retrieving data for the year %s", year))
    }

    if (year == 2022) {
        stop("CBSAs were not defined for 2022; choose a different year.")
    }

    if (year < 2011) {
        fname <- as.character(match.call())[[1]]

        msg <- sprintf(
            "%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.",
            fname
        )

        stop(msg, call. = FALSE)
    }

    if (!(resolution %in% c('500k', '5m', '20m'))) {
        stop(
            "Invalid value for resolution. Valid values are '500k', '5m', and '20m'.",
            call. = FALSE
        )
    }

    cyear <- as.character(year)

    if (cb == TRUE) {
        url <- sprintf(
            "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_csa_%s.zip",
            cyear,
            cyear,
            resolution
        )

        if (year == 2013) url <- gsub("shp/", "", url)
    } else {
        url <- sprintf(
            "https://www2.census.gov/geo/tiger/TIGER%s/CSA/tl_%s_us_csa.zip",
            cyear,
            cyear
        )
    }

    return(load_tiger(url, tigris_type = "csa", ...))
}

#' Download a metropolitan divisions shapefile into R.
#'
#' Metropolitan divisions are subdivisions of metropolitan areas with population of at least 2.5 million.  Please note:
#' not all metropolitan areas have metropolitan divisions.
#'
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family metro area functions
#' @seealso \url{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf}
#' @export
metro_divisions <- function(year = NULL, ...) {
    if (is.null(year)) {
        year <- getOption("tigris_year", 2024)

        message(sprintf("Retrieving data for the year %s", year))
    }

    if (year < 2011) {
        fname <- as.character(match.call())[[1]]

        msg <- sprintf(
            "%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.",
            fname
        )

        stop(msg, call. = FALSE)
    }

    cyear <- as.character(year)

    url <- sprintf(
        "https://www2.census.gov/geo/tiger/TIGER%s/METDIV/tl_%s_us_metdiv.zip",
        cyear,
        cyear
    )

    return(load_tiger(url, tigris_type = "metro", ...))
}

#' Download a New England City and Town Area shapefile into R
#'
#' From the US Census Bureau (see link for source): "In New England
#' (Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island,
#' and Vermont), the OMB has defined an alternative county subdivision
#' (generally city and town) based definition of CBSAs
#' known as New England city and town areas (NECTAs). NECTAs are defined using the same criteria as
#' metropolitan and micropolitan statistical areas and are identified as either metropolitan or micropolitan,
#' based, respectively, on the presence of either an urbanized area of 50,000 or more inhabitants or an
#' urban cluster of at least 10,000 and less than 50,000 inhabitants."  Combined NECTAs, or CNECTAs, are two or more
#' NECTAs that have significant employment interchange, like Combined Statistical Areas;
#' NECTA divisions are subdivisions of NECTAs.
#'
#' @param type Specify whether to download the New England City and Town Areas file (\code{'necta'}, the default),
#'        the combined NECTA file (\code{'combined'}), or the NECTA divisions file (\code{'divisions'}).
#' @param cb If cb is set to TRUE, download a generalized (1:500k)
#'        cartographic boundary file.  Defaults to FALSE (the most detailed
#'        TIGER/Line file).  Only available when \code{type = 'necta'}.
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family metro area functions
#' @seealso \url{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf}
#' @export
#' @examples \dontrun{
#' library(tigris)
#'
#' ne <- new_england(cb = TRUE)
#'
#' plot(ne$geometry)
#'
#' }
new_england <- function(type = 'necta', cb = FALSE, year = NULL, ...) {
    if (is.null(year)) {
        year <- getOption("tigris_year", 2024)

        message(sprintf("Retrieving data for the year %s", year))
    }

    if (year > 2021) {
        stop("New England areas are no longer defined for years after 2021.")
    }

    if (year < 2011) {
        fname <- as.character(match.call())[[1]]

        msg <- sprintf(
            "%s is not currently available for years prior to 2011.  To request this feature,
                   file an issue at https://github.com/walkerke/tigris.",
            fname
        )

        stop(msg, call. = FALSE)
    }

    cyear <- as.character(year)

    if (type == 'necta') {
        if (cb == TRUE) {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_necta_500k.zip",
                cyear,
                cyear
            )
        } else {
            url <- sprintf(
                "https://www2.census.gov/geo/tiger/TIGER%s/NECTA/tl_%s_us_necta.zip",
                cyear,
                cyear
            )
        }

        return(load_tiger(url, tigris_type = "necta", ...))
    } else if (type == 'combined') {
        url <- sprintf(
            "https://www2.census.gov/geo/tiger/TIGER%s/CNECTA/tl_%s_us_cnecta.zip",
            cyear,
            cyear
        )

        return(load_tiger(url, tigris_type = "cnecta", ...))
    } else if (type == 'divisions') {
        url <- sprintf(
            "https://www2.census.gov/geo/tiger/TIGER%s/NECTADIV/tl_%s_us_nectadiv.zip",
            cyear,
            cyear
        )

        return(load_tiger(url, tigris_type = "nectadiv", ...))
    } else {
        stop(
            "Invalid NECTA type; valid values include 'necta' (the default), 'combined', and 'divisions'.",
            call. = FALSE
        )
    }
}
