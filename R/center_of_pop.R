#' Download Census centers of population into R
#'
#' Description from the Census Bureau: "The concept of the center of population as used
#' by the U.S. Census Bureau is that of a balance point. The center of population is
#' the point at which an imaginary, weightless, rigid, and flat (no elevation effects)
#' surface representation of the 50 states (or 48 conterminous states for calculations
#' made prior to 1960) and the District of Columbia would balance if weights of identical
#' size were placed on it so that each weight represented the location of one person."
#' For more information, please see the link provided.
#'
#' Centers of population are published as text files for the 50 states, the
#' District of Columbia, and Puerto Rico; this function reads them and returns
#' an sf object of points in NAD 1983 (EPSG:4269). Areas with no population have
#' no center, so they are returned with empty geometries.
#'
#' @param geography One of `"state"`, `"county"`, `"tract"`, or `"block group"`.
#' @param state The state for which to download data. Required for county, tract,
#'   and block group centers, except 2000 tracts. State centers and 2000 tract
#'   centers come from national files, which are filtered to `state` if it is
#'   supplied.
#' @param year The decennial Census year: 2000, 2010, or 2020 (the default).
#' @return An sf object of points.
#' @seealso \url{https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.html}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(ggplot2)
#'
#' ctrs <- population_centers("county", state = "WA", year = 2020)
#' wa_counties <- counties(state = "WA", year = 2020)
#'
#' ggplot() +
#'   geom_sf(data = wa_counties, fill = "grey") +
#'   geom_sf(data = ctrs, color = "red")
#' }
population_centers <- function(
    geography = c("state", "county", "tract", "block group"),
    state = NULL,
    year = 2020
) {
    geography <- match.arg(geography)

    if (length(year) != 1 || !year %in% c(2000, 2010, 2020)) {
        cli_abort(
            "Centers of population are only available for the 2000, 2010, and 2020 Censuses."
        )
    }

    national <- geography == "state" || (geography == "tract" && year == 2000)

    state <- validate_state(state, require_state = !national)

    if (!is.null(state) && state %in% c("60", "66", "69", "74", "78")) {
        cli_abort(
            "Centers of population are only available for the 50 states, the District of Columbia, and Puerto Rico."
        )
    }

    if (year == 2000) {
        state_abb <- tolower(fips_codes$state[match(state, fips_codes$state_code)])
        path <- switch(
            geography,
            state = "statecenters.txt",
            county = sprintf("county/cou_%s_%s.txt", state, state_abb),
            tract = "tract/tract_pop.txt",
            "block group" = sprintf("blkgrp/bg_%s_%s.txt", state, state_abb)
        )
    } else {
        path <- switch(
            geography,
            state = sprintf("CenPop%s_Mean_ST.txt", year),
            county = sprintf("county/CenPop%s_Mean_CO%s.txt", year, state),
            tract = sprintf("tract/CenPop%s_Mean_TR%s.txt", year, state),
            "block group" = sprintf("blkgrp/CenPop%s_Mean_BG%s.txt", year, state)
        )
    }

    url <- sprintf(
        "https://www2.census.gov/geo/docs/reference/cenpop%s/%s",
        year,
        path
    )

    col_names <- switch(
        geography,
        state = c("STATEFP", "STNAME", "POPULATION", "LATITUDE", "LONGITUDE"),
        county = c("STATEFP", "COUNTYFP", "COUNAME", "STNAME", "POPULATION", "LATITUDE", "LONGITUDE"),
        tract = c("STATEFP", "COUNTYFP", "TRACTCE", "POPULATION", "LATITUDE", "LONGITUDE"),
        "block group" = c("STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE", "POPULATION", "LATITUDE", "LONGITUDE")
    )

    # The 2000 county files have no state name column
    if (year == 2000 && geography == "county") {
        col_names <- setdiff(col_names, "STNAME")
    }

    col_classes <- ifelse(
        col_names %in% c("LATITUDE", "LONGITUDE"),
        "numeric",
        ifelse(col_names == "POPULATION", "integer", "character")
    )

    # Download first, for retries and a clear error if the file is missing
    file <- tempfile(fileext = ".txt")
    on.exit(unlink(file), add = TRUE)
    tigris_download(url, file)

    if (year == 2000 && geography == "state") {
        dat <- utils::read.fwf(
            file,
            widths = c(5, 20, 10, 12, 12),
            skip = 4,
            col.names = col_names,
            colClasses = col_classes
        )
    } else {
        # 2000 and 2010 files are Latin-1; 2020 files are UTF-8 with a byte order mark
        dat <- utils::read.csv(
            file,
            header = year != 2000,
            col.names = col_names,
            colClasses = col_classes,
            # Census writes "+." / "-." coordinates for areas with no population
            na.strings = c("", "NA", "+.", "-."),
            fileEncoding = if (year == 2020) "UTF-8-BOM" else "latin1"
        )
    }

    dat[] <- lapply(dat, function(x) if (is.character(x)) trimws(x) else x)

    if (national && !is.null(state)) {
        dat <- dat[dat$STATEFP == state, ]
        rownames(dat) <- NULL

        if (nrow(dat) == 0) {
            cli_abort(
                "The {year} {geography} centers of population file has no data for this state."
            )
        }
    }

    sf::st_as_sf(
        dat,
        coords = c("LONGITUDE", "LATITUDE"),
        crs = 4269,
        na.fail = FALSE
    )
}
