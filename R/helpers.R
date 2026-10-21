#' Set the cache directory to store shapefiles with tigris
#'
#' @description By default, tigris uses the rappdirs package to determine a suitable location to store shapefiles
#' on the user's computer.  However, it is possible that the user would want to store shapefiles in a custom
#' location.  This function allows users to set the cache directory, and stores the result in the user's
#' .Renviron so that tigris will remember the location.
#'
#' Windows users: please note that you'll need to use double-backslashes or forward slashes
#' when specifying your cache directory's path in R.
#'
#' @param path The full path to the desired cache directory
#' @export
#' @examples \dontrun{
#' # Set the cache directory
#' tigris_cache_dir('PATH TO MY NEW CACHE DIRECTORY')
#'
#' # Check to see if it has been set correctly
#' Sys.getenv('TIGRIS_CACHE_DIR')
#' }
tigris_cache_dir <- function(path) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if (!file.exists(renv)) {
        file.create(renv)
    }

    check <- readLines(renv)

    if (isTRUE(any(grepl("TIGRIS_CACHE_DIR", check)))) {
        oldenv <- read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[!grepl("TIGRIS_CACHE_DIR", oldenv$V1), ]
        write.table(
            newenv,
            renv,
            quote = FALSE,
            sep = "\n",
            col.names = FALSE,
            row.names = FALSE
        )
    }

    var <- paste0("TIGRIS_CACHE_DIR=", "'", path, "'")

    write(var, renv, sep = "\n", append = TRUE)
    cli_bullets(
        c(
          "i" = "Your new tigris cache directory is {.path path}.",
          "*" = "To use now, restart R or run `readRenviron('~/.Renviron')`"
        )
    )
}


#' Helper function to download Census data
#'
#' @param url URL for zipped shapefile in TIGER database (constructed in calling function).
#' @param refresh Whether to re-download shapefiles if cached. Defaults to value of the global
#' option "tigris_refresh" if that option is, and FALSE if not. This will override the behavior
#' set in "tigris_refresh" option if a value (TRUE or FALSE) is provided.
#' @param tigris_type Added as an attribute to return object (used internally).
#' @param class Class of return object. Must be one of "sf" (the default) or "sp".
#' @param progress_bar If set to FALSE, do not display download progress bar
#' (helpful for R Markdown documents). Defaults to TRUE.
#' @param keep_zipped_shapefile If set to TRUE, do not delete zipped shapefile
#' (stored in temporary directory or TIGRIS_CACHE_DIR depending on the configuration of
#' global option "tigris_use_cache"). Defaults to FALSE.
#' @param query Optional OGR SQL query used to select features, passed to
#' [sf::st_read()]. Each TIGER/Line download contains a single layer named
#' after the zip file (e.g. `"tl_2025_us_county"`), and the name varies with
#' year, geography, and resolution, so use `{layer}` as a placeholder for it:
#' `"SELECT * FROM {layer} WHERE STATEFP = '24'"`. Defaults to `NA` (all
#' features). If `filter_by` is also supplied, the spatial filter is passed to
#' [sf::st_read()] alongside the query.
#' @param filter_by Geometry used to filter the output returned by the function.  Can be an sf object, an object of class `bbox`, or a length-4 vector of format `c(xmin, ymin, xmax, ymax)` that can be converted to a bbox. Geometries that intersect the input to `filter_by` will be returned.
#' @param geos_method Method passed to [sf::st_make_valid()] to repair
#' `filter_by` if it contains invalid geometries. Must be `"valid_linework"` or
#' `"valid_structure"` (the default). Ignored if `filter_by` is valid. Also
#' ignored (with a warning from sf) for longitude/latitude coordinates, which
#' includes the default NAD83, unless S2 is disabled with
#' `sf::sf_use_s2(FALSE)`.
#' @param crs Coordinate reference system to return. `NULL` and `NA` values are
#' ignored. Passed to [sf::st_transform()]. Defaults to "EPSG:4269" (unless set
#' using global option "tigris_crs").
#' @param protocol Character string specifying the protocol to use for downloading files. Options are "ftp" or "http" (default). If "ftp", the URL will be modified to use FTP instead of HTTPS.
#' @param timeout No longer used: downloads have no time limit. Kept so existing code that sets it keeps working.
#'
#' @return sf or sp data frame
#'
load_tiger <- function(
    url,
    refresh = getOption("tigris_refresh", FALSE),
    tigris_type = NULL,
    class = getOption("tigris_class", "sf"),
    progress_bar = TRUE,
    keep_zipped_shapefile = FALSE,
    query = NA,
    filter_by = NULL,
    geos_method = "valid_structure",
    crs = getOption("tigris_crs", "EPSG:4269"),
    protocol = getOption("tigris_protocol", "http"),
    timeout = NULL
) {
    # `timeout` is no longer used: downloads have no time limit. It's kept so
    # code that sets it keeps working.

    use_cache <- getOption("tigris_use_cache", FALSE)

    # Process filter_by
    wkt_filter <- input_to_wkt(filter_by, geos_method = geos_method)

    # HTTPS by default, with FTP as a fallback; FTP only if requested
    ftp_url <- gsub("^https://www2", "ftp://ftp2", url)
    if (protocol == "ftp") {
        url <- ftp_url
    }
    if (identical(ftp_url, url)) {
        ftp_url <- NULL
    }

    tiger_file <- basename(url)
    shape <- gsub(".zip", "", tiger_file)
    shape <- gsub("_shp", "", shape) # for historic tracts

    if (use_cache) {
        cache_dir <- get_tigris_cache_dir()
        file_loc <- file.path(cache_dir, tiger_file)
        shp_loc <- file.path(cache_dir, sprintf("%s.shp", shape))

        if (refresh || !file.exists(shp_loc)) {
            # Downloads are extracted and checked here before they replace
            # anything in the cache
            work_dir <- tempfile("staging-", tmpdir = cache_dir)
            dir.create(work_dir)
            on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

            staged <- tiger_fetch(url, ftp_url, file_loc, shape, work_dir, progress_bar)
            publish_layer(staged, shape, cache_dir)

            if (!keep_zipped_shapefile) {
                unlink(file_loc)
            }
        }

        dsn <- cache_dir
    } else {
        # The zip stays in tempdir(), as before; each call extracts into its
        # own directory
        file_loc <- file.path(tempdir(), tiger_file)
        work_dir <- tempfile("tigris-")
        dir.create(work_dir)
        on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

        dsn <- tiger_fetch(url, ftp_url, file_loc, shape, work_dir, progress_bar)
    }

    tiger_read(
        dsn,
        shape,
        query = query,
        wkt_filter = wkt_filter,
        crs = crs,
        tigris_type = tigris_type,
        class = class
    )
}

#' Download a TIGER/Line zip to `path` and extract it
#'
#' Tries `url`, then `ftp_url` if given. Each download counts as successful
#' only if it extracts and reads cleanly (see `extract_layer()`), so a corrupt
#' or truncated file is retried, and then falls back to FTP, like any other
#' failure. A file that doesn't exist on the server errors right away, as the
#' FTP server has the same files.
#'
#' @return The directory inside `work_dir` holding the extracted, checked layer.
#' @noRd
tiger_fetch <- function(url, ftp_url, path, layer, work_dir, progress_bar = TRUE) {
    extracted <- NULL

    # Extract each downloaded file into its own directory and keep the one
    # that checks out
    check <- function(zip) {
        dir <- tempfile("extract-", tmpdir = work_dir)
        dir.create(dir)
        if (extract_layer(zip, layer, dir)) {
            extracted <<- dir
            return(TRUE)
        }
        unlink(dir, recursive = TRUE)
        FALSE
    }

    first <- tryCatch(
        tigris_download(url, path, validate = check, progress = progress_bar),
        error = function(e) e
    )

    if (!inherits(first, "error")) {
        return(extracted)
    }

    if (inherits(first, "tigris_file_not_found")) {
        stop(first)
    }

    if (is.null(ftp_url)) {
        cli_abort(
            c(
                "Download failed; check your internet connection or the status of the Census Bureau website
             at https://www2.census.gov/geo/tiger/ or ftp://ftp2.census.gov/geo/tiger/.",
                "i" = "{conditionMessage(first)}"
            ),
            call = NULL
        )
    }

    cli_bullets("HTTP download failed, trying FTP as fallback...")

    second <- tryCatch(
        tigris_download(ftp_url, path, validate = check, progress = progress_bar),
        error = function(e) e
    )

    if (inherits(second, "error")) {
        cli_abort(
            c(
                "Download failed with both HTTP and FTP; check your internet connection or the status of the Census Bureau website
                 at https://www2.census.gov/geo/tiger/ or ftp://ftp2.census.gov/geo/tiger/.",
                "i" = "HTTP: {conditionMessage(first)}",
                "i" = "FTP: {conditionMessage(second)}"
            ),
            call = NULL
        )
    }

    extracted
}

#' Extract a TIGER/Line zip into `dir` and check that its layer reads cleanly
#'
#' Every file must extract without errors, the shapefile's .shp, .shx, and .dbf
#' must be present, and GDAL must read every feature without errors.
#' zip::unzip() verifies each file's checksum; utils::unzip() doesn't, so it
#' can extract damaged data without complaint.
#' @noRd
extract_layer <- function(zip, layer, dir) {
    extracted <- tryCatch(
        {
            zip::unzip(zip, exdir = dir)
            TRUE
        },
        warning = function(w) FALSE,
        error = function(e) FALSE
    )

    required <- file.path(dir, paste0(layer, c(".shp", ".shx", ".dbf")))
    if (!extracted || !all(file.exists(required))) {
        return(FALSE)
    }

    gdal_error <- FALSE
    obj <- tryCatch(
        withCallingHandlers(
            st_read(dir, layer = layer, quiet = TRUE),
            warning = function(w) {
                if (grepl("GDAL Error", conditionMessage(w))) gdal_error <<- TRUE
                invokeRestart("muffleWarning")
            }
        ),
        error = function(e) NULL
    )

    !is.null(obj) && !gdal_error
}

#' Move a checked layer from `staged` into `dest_dir`
#'
#' Replaces the layer's existing files in `dest_dir`, so old and new files are
#' never mixed. If a move fails partway, the old files are restored.
#' @noRd
publish_layer <- function(staged, layer, dest_dir) {
    new_files <- list.files(staged)

    # The layer's existing files (by exact layer name), apart from the zip
    old_files <- list.files(dest_dir)
    old_files <- old_files[
        startsWith(old_files, paste0(layer, ".")) & !endsWith(old_files, ".zip")
    ]

    backup <- tempfile("backup-", tmpdir = dest_dir)
    dir.create(backup)
    moved_old <- character()
    moved_new <- character()

    restore <- function() {
        unlink(file.path(dest_dir, moved_new))
        restored <- vapply(
            moved_old,
            function(f) move_file(file.path(backup, f), file.path(dest_dir, f)),
            logical(1)
        )
        if (all(restored)) {
            unlink(backup, recursive = TRUE)
            cli_abort(
                "Unable to update the cached files for {.val {layer}}; the previous files were kept.",
                call = NULL
            )
        }
        cli_abort(
            "Unable to update the cached files for {.val {layer}}; the previous files are in {.path {backup}}.",
            call = NULL
        )
    }

    for (f in old_files) {
        if (!move_file(file.path(dest_dir, f), file.path(backup, f))) restore()
        moved_old <- c(moved_old, f)
    }

    for (f in new_files) {
        if (!move_file(file.path(staged, f), file.path(dest_dir, f))) restore()
        moved_new <- c(moved_new, f)
    }

    unlink(backup, recursive = TRUE)

    invisible(dest_dir)
}

# file.rename(), wrapped so tests can simulate a failed move
move_file <- function(from, to) {
    file.rename(from, to)
}

#' Substitute the layer name into an OGR SQL query
#'
#' Each TIGER/Line download has one layer, named after the shapefile, so
#' `{layer}` in `query` is replaced with the quoted layer name.
#' @noRd
as_tigris_query <- function(query, layer, call = caller_env()) {
    if (is.null(query) || all(is.na(query))) {
        return(NA_character_)
    }

    if (!is.character(query) || length(query) != 1) {
        cli_abort("{.arg query} must be a single string or {.code NA}.", call = call)
    }

    gsub("{layer}", sprintf('"%s"', layer), query, fixed = TRUE)
}

#' Read a downloaded TIGER/Line shapefile and tidy it for tigris
#' @noRd
tiger_read <- function(
    dsn,
    layer,
    query = NA,
    wkt_filter = character(0),
    crs = getOption("tigris_crs", "EPSG:4269"),
    tigris_type = NULL,
    class = getOption("tigris_class", "sf")
) {
    query <- as_tigris_query(query, layer)

    # sf warns if layer is supplied along with a query
    read_args <- list(
        dsn = dsn,
        quiet = TRUE,
        stringsAsFactors = FALSE,
        wkt_filter = wkt_filter
    )

    if (is.na(query)) {
        read_args$layer <- layer
    } else {
        read_args$query <- query
    }

    obj <- do.call(st_read, read_args)

    if (is.na(st_crs(obj)$proj4string)) {
        st_crs(obj) <- "+proj=longlat +datum=NAD83 +no_defs"
    }

    if (!is.null(crs) && !is.na(crs) && (sf::st_crs(obj) != sf::st_crs(crs))) {
        obj <- sf::st_transform(obj, crs = crs)
    }

    attr(obj, "tigris") <- "tigris"

    # this will help identify the object "sub type"
    if (!is.null(tigris_type)) {
        attr(obj, "tigris") <- tigris_type
    }

    # Take care of COUNTYFP, STATEFP issues for historic data, keeping a file's
    # own COUNTYFP / STATEFP if it has them (e.g. 2011-2013 blocks)
    has_countyfp <- "COUNTYFP" %in% names(obj)
    if ("COUNTYFP00" %in% names(obj)) {
        obj$COUNTYFP <- obj$COUNTYFP00
        obj$STATEFP <- obj$STATEFP00
    }
    if ("COUNTYFP10" %in% names(obj) && !has_countyfp) {
        obj$COUNTYFP <- obj$COUNTYFP10
        obj$STATEFP <- obj$STATEFP10
    }
    if ("COUNTY" %in% names(obj)) {
        obj$COUNTYFP <- obj$COUNTY
        obj$STATEFP <- obj$STATE
    }
    if ("CO" %in% names(obj)) {
        obj$COUNTYFP <- obj$CO
        obj$STATEFP <- obj$ST
    }

    if (class == "sp") {
        cli_warn(
            c("Spatial* (sp) classes are no longer formally supported in tigris as of version 2.0.",
            "We strongly recommend updating your workflow to use sf objects (the default in tigris) instead.")
        )
        return(sf::as_Spatial(obj))
    } else {
        return(obj)
    }
}

#' Cache directory: TIGRIS_CACHE_DIR if set, otherwise the user cache directory
#' @noRd
get_tigris_cache_dir <- function() {
    if (Sys.getenv("TIGRIS_CACHE_DIR") != "") {
        cache_dir <- Sys.getenv("TIGRIS_CACHE_DIR")
        cache_dir <- path.expand(cache_dir)
    } else {
        cache_dir <- user_cache_dir("tigris")
    }
    if (!file.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
    }

    cache_dir
}

#' Easily merge a data frame to a spatial data frame
#'
#' This function should be considered deprecated.  Please update your workflow to use sf objects and dplyr's \code{*_join()} family of functions instead.
#'
#' @param spatial_data A spatial data frame to which you want to merge data.
#' @param data_frame A regular data frame that you want to merge to your spatial data.
#' @param by_sp The column name you'll use for the merge from your spatial data frame.
#' @param by_df The column name you'll use for the merge from your regular data frame.
#' @param by (optional) If a named argument is supplied to the by parameter, geo_join will assume that the join columns in the spatial data and data frame share the same name.
#' @param how The type of join you'd like to perform.  The default, 'left', keeps all rows in the spatial data frame, and returns NA for unmatched rows.  The alternative, 'inner', retains only those rows in the spatial data frame that match rows from the target data frame.
#' @return a joined spatial/data frame object
#' @export
#' @examples \dontrun{
#'
#' library(rnaturalearth)
#' library(WDI)
#' library(tigris)
#'
#' dat <- WDI(country = "all", indicator = "SP.DYN.LE00.IN", start = 2012, end = 2012)
#'
#' dat$SP.DYN.LE00.IN <- round(dat$SP.DYN.LE00.IN, 1)
#'
#' countries <- ne_countries()
#'
#' countries2 <- geo_join(countries, dat, 'iso_a2', 'iso2c')
#'
#' nrow(countries2)
#'
#' ## [1] 177
#'
#' countries3 <- geo_join(countries, dat, 'iso_a2', 'iso2c', how = 'inner')
#'
#' nrow(countries3)
#'
#' ## [1] 169
#'
#' }
geo_join <- function(
    spatial_data,
    data_frame,
    by_sp,
    by_df,
    by = NULL,
    how = 'left'
) {
    .Deprecated(
        "dplyr::left_join()",
        package = "tigris",
        msg = "We recommend using the dplyr::*_join() family of functions instead."
    )

    if (!is.null(by)) {
        by_sp <- by
        by_df <- by
    }

    # For sp objects
    if (
        class(spatial_data)[1] %in%
            c(
                "SpatialGridDataFrame",
                "SpatialLinesDataFrame",
                "SpatialPixelsDataFrame",
                "SpatialPointsDataFrame",
                "SpatialPolygonsDataFrame"
            )
    ) {
        spatial_data@data <- data.frame(
            spatial_data@data,
            data_frame[match(spatial_data@data[[by_sp]], data_frame[[by_df]]), ]
        )

        if (how == 'inner') {
            matches <- match(spatial_data@data[[by_sp]], data_frame[[by_df]])

            spatial_data <- spatial_data[!is.na(matches), ]

            return(spatial_data)
        } else if (how == 'left') {
            return(spatial_data)
        } else {
            cli_abort(
                "The available options for `how` are 'left' and 'inner'."
            )
        }

        # For sf objects
    } else if ("sf" %in% class(spatial_data)) {
        join_vars <- c(by_df)

        names(join_vars) <- by_sp

        if (how == "inner") {
            joined <- spatial_data %>%
                inner_join(data_frame, by = join_vars) %>%
                st_as_sf()

            attr(joined, "tigris") <- tigris_type(spatial_data)

            return(joined)
        } else if (how == "left") {
            # Account for potential duplicate rows in data frame
            df_unique <- data_frame %>%
                group_by_(by_df) %>%
                mutate(rank = row_number()) %>%
                filter(rank == 1)

            joined <- spatial_data %>%
                left_join(df_unique, by = join_vars) %>%
                st_as_sf()

            if (!is.na(st_crs(spatial_data)$epsg)) {
                crs <- st_crs(spatial_data)$epsg
            } else {
                crs <- st_crs(spatial_data)$proj4string
            }

            st_crs(joined) <- crs # re-assign the CRS

            attr(joined, "tigris") <- tigris_type(spatial_data)

            return(joined)
        } else {
            cli_abort(
                "The available options for `how` are 'left' and 'inner'."
            )
        }
    }
}


#' Look up state and county codes
#'
#' Function to look up the FIPS codes for states and optionally counties you'd l
#' ike to load data for.  As the package functions require the codes to return
#' the data correctly, this function makes it easy to find the codes that you need.
#'
#' @param state String representing the state you'd like to look up.
#'        Accepts state names (spelled correctly), e.g. "Texas", or
#'        postal codes, e.g. "TX". Can be lower-case.
#' @param county The name of the county you'll like to search for.  T
#'        he state that the county is located in must be supplied for this to
#'        work, as there are multiple counties with the same names across states.
#'        Can be lower-case.
#' @return character string with an explanation of state/county FIPS codes
#' @export
#' @examples \dontrun{
#' lookup_code("me")
#' ## [1] "The code for Maine is '23'."
#'
#' lookup_code("Maine")
#' ## [1] "The code for Maine is '23'."
#'
#' lookup_code("23")
#' ## [1] "The code for Maine is '23'."
#'
#' lookup_code(23)
#' ## [1] "The code for Maine is '23'."
#'
#' lookup_code("me", "york")
#' ## [1] "The code for Maine is '23' and the code for York County is '031'."
#'
#' lookup_code("Maine", "York County")
#' ## [1] "The code for Maine is '23' and the code for York County is '031'."
#' }
lookup_code <- function(state, county = NULL) {
    state <- validate_state(state, .msg = FALSE, require_state = TRUE)

    fips_codes <- tigris::fips_codes

    if (!is.null(county)) {
        vals <- fips_codes[
            fips_codes$state_code == state &
                grepl(
                    sprintf("^%s", county),
                    fips_codes$county,
                    ignore.case = TRUE
                ),
        ]

        return(paste0(
            "The code for ",
            vals$state_name,
            " is '",
            vals$state_code,
            "'",
            " and the code for ",
            vals$county,
            " is '",
            vals$county_code,
            "'."
        ))
    } else {
        vals <- head(fips_codes[fips_codes$state_code == state, ], 1)

        return(paste0(
            "The code for ",
            vals$state_name,
            " is '",
            vals$state_code,
            "'."
        ))
    }
}

#' Returns \code{TRUE} if \code{obj} has a \code{tigris} attribute
#'
#' It's unlikely that said object was not created by this package
#'
#' @param obj R object to test
#' @return \code{TRUE} if \code{obj} was made by this package
#' @export
is_tigris <- function(obj) {
    !is.null(attr(obj, "tigris"))
}

#' Get the type of \code{tigris} object \code{obj} is
#'
#' @param obj R object to test
#' @return character vector containing the \code{tigris} type of \code{obj}
#'         or \code{NA} if \code{obj} is not a code \code{tigris} object
#' @export
tigris_type <- function(obj) {
    if (is_tigris(obj)) {
        return(attr(obj, "tigris"))
    }
    return(NA)
}

#' Return a data frame of county names & FIPS codes for a given state
#'
#' @param state String representing the state you'd like to look up.
#'        Accepts state names (spelled correctly), e.g. "Texas", or
#'        postal codes, e.g. "TX". Can be lower-case.
#' @return data frame of county name and FIPS code or NULL if invalid state
#' @export
list_counties <- function(state) {
    state <- validate_state(state, .msg = FALSE, require_state = TRUE)

    fips_codes <- tigris::fips_codes

    vals <- fips_codes[
        fips_codes$state_code == state,
        c("county", "county_code")
    ]
    vals$county <- gsub("\ County$", "", vals$county)
    rownames(vals) <- NULL
    return(vals)
}

#' Row-bind \code{tigris} Spatial objects
#'
#' If multiple school district types are rbound, coerces to "sdall" and does it
#'
#' @param ... individual (optionally names) \code{tigris} Spatial objects or a list of them
#' @return one combined Spatial object
#' @export
#' @examples \dontrun{
#' library(tigris)
#'
#' me_ctys <- list_counties("me")
#' aw <- lapply(me_ctys$county_code[1:3], function(x) {
#'   area_water("Maine", x)
#' }) %>%
#'   rbind_tigris()
#' }

rbind_tigris <- function(...) {
    elements <- list(...)

    if (
        (length(elements) == 1) &
            inherits(elements, "list")
    ) {
        elements <- unlist(elements, recursive = FALSE) # Necessary given structure of sf objects
    }

    obj_classes <- unique(sapply(elements, class))
    obj_attrs <- sapply(elements, attr, "tigris")
    obj_attrs_u <- unique(obj_attrs)

    if (
        any(
            c(
                "SpatialGridDataFrame",
                "SpatialLinesDataFrame",
                "SpatialPixelsDataFrame",
                "SpatialPointsDataFrame",
                "SpatialPolygonsDataFrame"
            ) %in%
                obj_classes
        ) &
            "sf" %in% obj_classes
    ) {
        cli_abort("Cannot combine sp and sf objects")
    }

    #handling for attempts to rbind disparate school districts

    if (all(obj_attrs %in% c("unsd", "elsd", "scsd"))) {
        # 3 school district types

        cli_warn(
            "Multiple school district tigris types. Coercing to {.str sdall}."
        )

        elements <- lapply(seq_along(elements), function(x) {
            names(elements[[x]])[2] <- "SDLEA" # Used in some spots elsewhere in TIGER
            attr(elements[[x]], "tigris") <- "sdall" # New type
            elements[[x]]
        })

        obj_attrs <- sapply(elements, attr, "tigris")
        obj_attrs_u <- unique(obj_attrs)
    }

    # all same type
    # all valid Spatial* type
    # none are from outside tigris
    # all same tigris "type"

    if (
        obj_classes[1] %in%
            c(
                "SpatialGridDataFrame",
                "SpatialLinesDataFrame",
                "SpatialPixelsDataFrame",
                "SpatialPointsDataFrame",
                "SpatialPolygonsDataFrame"
            )
    ) {
        cli_abort(
            "Spatial* classes are no longer supported in tigris as of version 2.0. You will need to install an earlier version of tigris with `remotes::install_version()`."
        )
    } else if ("sf" %in% obj_classes) {
        crs <- unique(sapply(elements, function(x) {
            return(st_crs(x)$epsg)
        }))

        if (length(crs) > 1) {
            cli_abort(
                "All objects must share a single coordinate reference system."
            )
        }

        if (
            !any(sapply(obj_attrs, is.null)) &
                length(obj_attrs_u) == 1
        ) {
            geometries <- unlist(lapply(elements, function(x) {
                geoms <- st_geometry_type(x)
                unique(geoms)
            }))

            # Cast polygon to multipolygon to allow for rbind-ing
            # This will need to be checked for linear objects as well
            if ("POLYGON" %in% geometries & "MULTIPOLYGON" %in% geometries) {
                elements <- lapply(elements, function(x) {
                    st_cast(x, "MULTIPOLYGON")
                })
            }

            if (
                "LINESTRING" %in% geometries & "MULTILINESTRING" %in% geometries
            ) {
                elements <- lapply(elements, function(x) {
                    st_cast(x, "MULTILINESTRING")
                })
            }

            tmp <- dplyr::bind_rows(elements)

            # Re-assign the original CRS if missing
            if (is.na(st_crs(tmp)$proj4string)) {
                st_crs(tmp) <- crs
            }

            attr(tmp, "tigris") <- obj_attrs_u
            return(tmp)
        } else {
            cli_abort(
                "Objects must all be the same type of tigris object."
            )
        }
    }
}

#' Erase water area from an input polygon dataset
#'
#' This function 'erases' water area from an input polygon dataset (typically a Census dataset).
#' 'Erase' is defined in the traditional GIS sense as the removal of areas in an input layer
#' from an erase layer, returning the modified input layer. A common use-case is to improve
#' cartographic representation of locations where US Census polygons include more water area
#' than desired (e.g. New York City, Seattle) or to support contiguity-based spatial analyses that
#' might otherwise incorrectly assume that polygons across bodies of water are neighbors.
#'
#' The function works by identifying US counties that intersect the input polygon layer,
#' then requesting water polygons (using \code{tigris::area_water()}) to be erased from
#' those input polygons. The \code{area_threshold} parameter can be tuned to determine the
#' percentile ranking of bodies of water (by area) to use;
#' the default is a percentile ranking of 0.75, erasing the largest 25 percent of water
#' bodies in the region.
#'
#' Analysts will ideally have transformed the input coordinate reference system (CRS) of their data
#' to a projected CRS to improve performance; see \url{https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html#coordinate-reference-systems} for more information on
#' how to perform CRS transformations.  Analysts should also use this function with caution;
#' the function may generate sliver polygons or irregular geometries in the output layer,
#' especially if the input sf object was not obtained with the tigris package.  Also, the operation
#' may be quite slow for large input areas.
#'
#' @param input_sf An input sf object, ideally obtained with the tigris package or through tidycensus.
#' @param area_threshold The percentile rank cutoff of water areas to use in the erase operation, ranked by size. Defaults to 0.75, representing the water areas in the 75th percentile and up (the largest 25 percent of areas).  This value may need to be modified by the user to achieve optimal results for a given location.
#' @param year The year to use for the water layer (2011 or later); defaults to 2025 unless the \code{tigris_year} option is otherwise set.
#'
#' @return An output sf object representing the polygons in \code{input_sf} with water areas erased.
#' @export
#'
#' @examples \dontrun{
#'
#' library(tigris)
#' library(sf)
#' options(tigris_use_cache = TRUE)
#'
#' king_tracts <- tracts(state = "WA", county = "King", year = 2020)
#'
#' # CRS: NAD 1983 / Washington North (State Plane)
#' king_erased <- king_tracts %>%
#'     st_transform(32148) %>%
#'     erase_water(area_threshold = 0.9)
#'
#' plot(king_erased$geometry)
#' }
erase_water <- function(input_sf, area_threshold = 0.75, year = NULL) {
    # if (is.null(attr(input_sf, "tigris"))) {
    #   warning("Your input object does not appear to have been obtained with the tigris package.\nThis is likely to introduce sliver polygons or irregular geometries.\nPlease proceed with caution.")
    # }

    if (!"sf" %in% class(input_sf)) {
        cli_abort("The input dataset is not an sf object.")
    }

    # area_water() is available from 2011
    year <- set_tigris_year(year, min_year = 2011, quiet = TRUE)

    # Define st_erase function internally
    st_erase <- function(x, y) {
        suppressWarnings(sf::st_difference(x, sf::st_union(y)))
    }

    # Grab a dataset of counties quietly; there are no cartographic boundary
    # county files for 2011 and 2012
    us_counties <- tigris::counties(
        cb = year >= 2013,
        resolution = "500k",
        progress_bar = FALSE,
        year = year
    )

    # Identify the counties that overlap the input sf object
    county_overlay <- us_counties %>%
        sf::st_transform(sf::st_crs(input_sf)) %>%
        sf::st_filter(input_sf)

    # If nothing returned, exit
    if (nrow(county_overlay) == 0) {
        cli_abort(
            "Your dataset does not appear to be in the United States; this function is not appropriate for your data."
        )
    }

    # Get a list of GEOIDs
    county_GEOIDs <- county_overlay$GEOID

    # Fetch water for those GEOIDs
    cli_bullets("Fetching area water data for your dataset's location...")
    my_water <- lapply(county_GEOIDs, function(cty) {
        suppressMessages(tigris::area_water(
            state = stringr::str_sub(cty, 1, 2),
            county = stringr::str_sub(cty, 3, 5),
            progress_bar = FALSE,
            year = year
        ))
    }) %>%
        dplyr::bind_rows() %>%
        sf::st_transform(sf::st_crs(input_sf)) %>%
        sf::st_filter(input_sf) %>% # New step to only erase intersecting water areas
        dplyr::filter(dplyr::percent_rank(AWATER) >= area_threshold)

    if (nrow(my_water) == 0) {
        cli_bullets(
            c(
                "No overlapping water area found.",
                "i" = "Returning unmodified input."
            )
        )
        return(input_sf)
    }

    cli_bullets(
        "Erasing water area...\nIf this is slow, try a larger area threshold value."
    )
    erased_sf <- suppressMessages(st_erase(input_sf, my_water))

    # The erase can leave invalid geometries (e.g. duplicate vertices), which
    # break later operations like st_area()
    if (!isTRUE(all(sf::st_is_valid(erased_sf)))) {
        erased_sf <- sf::st_make_valid(erased_sf)
    }

    return(erased_sf)
}


#' Documentation Template for Functions that Utilize `load_tiger`
#'
#' @md
#' @details This documentation is inherited by `tigris` functions that wrap `load_tiger`.
#' Include the following in function documentation:
#' `@inheritParams load_tiger_doc_template`
#' `@inheritSection load_tiger_doc_template Additional Arguments`
#'
#' @param ... arguments to be passed to internal function `load_tiger`, which is not exported. See Additional Arguments.
#' @param year the data year; defaults to 2025
#'
#' @section Additional Arguments:
#' Additional arguments that can be passed in `...` are:
#'  * `class` Desired class of return object: `"sf"` (the default) or `"sp"`.  sp classes should be considered deprecated as of tigris version 2.0, but legacy support is still available.
#'  * `progress_bar` If set to `FALSE`, do not display download progress bar (helpful for R Markdown documents). Defaults to `TRUE`.
#'  * `keep_zipped_shapefile` If set to `TRUE`, do not delete zipped shapefile (stored in temporary directory or `TIGRIS_CACHE_DIR`
#'     depending on the configuration of global option `"tigris_use_cache"`). Defaults to `FALSE`.
#'  * `refresh` Whether to re-download cached shapefiles (`TRUE` or `FALSE`) . The default is either `FALSE` or the value of global
#'     option `"tigris_refresh"` if it is set. Specifying this argument will override the behavior set in `"tigris_refresh"` global option.
#'  * `filter_by` Geometry used to filter the output returned by the function.  Can be an sf object, an object of class `bbox`, or a length-4 vector of format `c(xmin, ymin, xmax, ymax)` that can be converted to a bbox. Geometries that intersect the input to `filter_by` will be returned.
#'  * `protocol` Character string specifying the protocol to use for downloading files. Options are "ftp" or "http" (default). If "ftp", the URL will be modified to use FTP instead of HTTPS.
#'  * `timeout` No longer used: downloads have no time limit. Kept so existing code that sets it keeps working.
#' @keywords internal
load_tiger_doc_template <- function(..., year) NULL
