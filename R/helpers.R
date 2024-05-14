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
    write.table(newenv, renv, quote = FALSE, sep = "\n",
                col.names = FALSE, row.names = FALSE)
  }

  var <- paste0("TIGRIS_CACHE_DIR=", "'", path, "'")

  write(var, renv, sep = "\n", append = TRUE)
  inform(
    c(
      "i" = sprintf("Your new tigris cache directory is %s.", path),
      "*" = "To use now, restart R or run `readRenviron('~/.Renviron')`"
    )
  )
}


#' Helper function to download Census data
#'
#' @param url URL for zipped shapefile in TIGER database (constructed in calling
#'   function).
#' @param refresh Whether to re-download shapefiles if cached. Defaults to value
#'   of the global option "tigris_refresh" if that option is, and FALSE if not.
#'   This will override the behavior set in "tigris_refresh" option if a value
#'   (TRUE or FALSE) is provided.
#' @param tigris_type Added as an attribute to return object (used internally).
#' @param class Class of return object. Must be one of "sf" (the default) or
#'   "sp".
#' @param progress_bar If set to FALSE, do not display download progress bar
#' (helpful for R Markdown documents). Defaults to TRUE.
#' @param keep_zipped_shapefile If set to TRUE, do not delete zipped shapefile
#' (stored in temporary directory or TIGRIS_CACHE_DIR depending on the
#' configuration of global option "tigris_use_cache"). Defaults to FALSE.
#' @param filter_by Geometry used to filter the output returned by the function.
#'   Can be an sf object, sfc object, an object of class `bbox`, or a length-4
#'   vector of format `c(xmin, ymin, xmax, ymax)` that can be converted to a
#'   bbox. Geometries that intersect the input to `filter_by` will be returned.
#' @inheritParams rlang::args_error_context
#'
#' @return sf or sp data frame
#'
load_tiger <- function(url,
                       refresh = getOption("tigris_refresh", FALSE),
                       tigris_type = NULL,
                       class = getOption("tigris_class", "sf"),
                       progress_bar = TRUE,
                       keep_zipped_shapefile = FALSE,
                       filter_by = NULL,
                       call = caller_env()) {
  # Process filter_by
  wkt_filter <- input_to_wkt(filter_by, call = call)

  tiger_file <- basename(url)

  obj <- NULL

  use_cache <- getOption("tigris_use_cache", FALSE)

  if (use_cache) {

    cache_dir <- get_tigris_cache_dir()

    if (dir.exists(cache_dir)) {

      shape <- gsub(".zip", "", tiger_file)
      shape <- gsub("_shp", "", shape) # for historic tracts

      file_loc <- file.path(cache_dir, tiger_file)
      shp_loc  <- file.path(cache_dir, sprintf("%s.shp", shape))

      if (refresh || !file.exists(shp_loc)) {

        check_tigris_url(url, call = call)

        if (progress_bar) {
          try(GET(url,
                  write_disk(file_loc, overwrite=refresh),
                  progress(type="down")), silent=TRUE)
        } else {
          try(GET(url,
                  write_disk(file_loc, overwrite=refresh)),
                  silent=TRUE)
        }

        unzip_tiger <- function() {
          unzip(file_loc, exdir = cache_dir, overwrite=TRUE)
        }
        remove_zip_tiger <- function() {
          if (file.exists(file_loc) && file.exists(shp_loc)) {
            invisible(file.remove(file_loc))
          }
        }

        # Logic for handling download errors and re-downloading
        t <- tryCatch(unzip_tiger(), warning = function(w) w)

        if ("warning" %in% class(t)) {

          i <- 1

          while (i < 4) {

            inform(sprintf("Previous download failed. Re-download attempt %s of 3...",
                            as.character(i)))

            if (progress_bar) {
              try(GET(url,
                      write_disk(file_loc, overwrite=TRUE),
                      progress(type="down")), silent=TRUE)
            } else {
              try(GET(url,
                      write_disk(file_loc, overwrite=TRUE)),
                      silent=TRUE)
            }

            t <- tryCatch(unzip_tiger(), warning = function(w) w)

            if ("warning" %in% class(t)) {
              i <- i + 1
            } else {
              break
            }

          }

          if (i == 4) {
            abort(
              c(
                "Download failed",
                "*" = "Check your internet connection or the status of
                the Census Bureau website at http://www2.census.gov/geo/tiger/"
              ),
              call = call
            )
          }

        } else {

          unzip_tiger()

          if (!keep_zipped_shapefile) {

            remove_zip_tiger()

          }

        }

      }

      dsn <- cache_dir
    }

  } else {

    check_tigris_url(url, call = call)

    tmp <- tempdir()
    file_loc <- file.path(tmp, tiger_file)

    if (progress_bar) {
      try(GET(url, write_disk(file_loc),
              progress(type = "down")), silent = TRUE)
    } else {
      try(GET(url, write_disk(file_loc)),
              silent = TRUE)
    }

    unzip(file_loc, exdir = tmp)
    shape <- gsub(".zip", "", tiger_file)
    shape <- gsub("_shp", "", shape) # for historic tracts
    dsn <- tmp

  }

  obj <- st_read(dsn = dsn, layer = shape,
                 quiet = TRUE, stringsAsFactors = FALSE,
                 wkt_filter = wkt_filter)

  if (is.na(st_crs(obj)$proj4string)) {
    st_crs(obj) <- "+proj=longlat +datum=NAD83 +no_defs"
  }

  attr(obj, "tigris") <- "tigris"

  # this will help identify the object "sub type"
  if (!is.null(tigris_type)) attr(obj, "tigris") <- tigris_type

  obj <- rename_fips_cols(obj)

  if (class == "sp") {
    warn(
      c(
        "!" = "Spatial* (sp) classes are no longer formally supported in tigris as of version 2.0.",
        "i" = "We strongly recommend updating your workflow to use sf objects (the default in tigris) instead."
        )
     )
    return(sf::as_Spatial(obj))
  } else {
    return(obj)
  }
}

#' @noRd
#' @importFrom rappdirs user_cache_dir
get_tigris_cache_dir <- function() {
  if (Sys.getenv("TIGRIS_CACHE_DIR") != "") {
    cache_dir <- Sys.getenv("TIGRIS_CACHE_DIR")
    cache_dir <- path.expand(cache_dir)
  } else {
    cache_dir <- rappdirs::user_cache_dir("tigris")
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_dir
}

#' Check URL
#'
#' @noRd
check_tigris_url <- function(url, call = caller_env()) {
  if (!is_string(url)) {
    abort("`url` must be a string", call = call)
  }

  resp <- HEAD(url)

  if (http_error(resp)) {
    abort(
      paste0("`url` can't be found: ", url),
      call = call
    )
  }
}

#' Rename FIPS columns for historic data
#'
#' @noRd
rename_fips_cols <- function(obj) {
  # Take care of COUNTYFP, STATEFP issues for historic data
  if ("COUNTYFP00" %in% names(obj)) {
    obj$COUNTYFP <- obj$COUNTYFP00
    obj$STATEFP <- obj$STATEFP00
  }
  if ("COUNTYFP10" %in% names(obj)) {
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

  obj
}

#' Easily merge a data frame to a spatial data frame
#'
#' This function should be considered deprecated.  Please update your workflow to use sf objects and dplyr's `*_join()` family of functions instead.
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
geo_join <- function(spatial_data, data_frame, by_sp, by_df, by = NULL, how = "left") {

  .Deprecated("dplyr::left_join()",
              package = "tigris",
              msg = "We recommend using the dplyr::*_join() family of functions instead.")

  if (!is.null(by)) {
    by_sp <- by
    by_df <- by
  }

  how <- arg_match(how, c("inner", "left"))

  # For sp objects
  if (class(spatial_data)[1] %in% c("SpatialGridDataFrame", "SpatialLinesDataFrame",
                                "SpatialPixelsDataFrame", "SpatialPointsDataFrame",
                                "SpatialPolygonsDataFrame")) {

    spatial_data@data <- data.frame(spatial_data@data,
                                    data_frame[match(spatial_data@data[[by_sp]],
                                                     data_frame[[by_df]]), ])

    if (how == "inner") {

      matches <- match(spatial_data@data[[by_sp]], data_frame[[by_df]])

      spatial_data <- spatial_data[!is.na(matches), ]

      return(spatial_data)

    } else if (how == "left") {

      return(spatial_data)

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

  state <- validate_state(state, allow_null = FALSE, .msg = FALSE)

  if (!is.null(county)) {

    vals <- fips_codes[fips_codes$state_code == state &
                         grepl(sprintf("^%s", county), fips_codes$county, ignore.case=TRUE),]

    return(paste0("The code for ", vals$state_name,
                  " is '", vals$state_code, "'", " and the code for ",
                  vals$county, " is '", vals$county_code, "'."))

  } else {

    vals <- head(fips_codes[fips_codes$state_code == state,], 1)

    return(paste0("The code for ", vals$state_name, " is '", vals$state_code, "'."))

  }

}

#' Returns `TRUE` if `obj` has a `tigris` attribute
#'
#' It's unlikely that said object was not created by this package
#'
#' @param obj R object to test
#' @return `TRUE` if `obj` was made by this package
#' @export
is_tigris <- function(obj) { !is.null(attr(obj, "tigris")) }

#' Get the type of `tigris` object `obj` is
#'
#' @param obj R object to test
#' @return character vector containing the `tigris` type of `obj`
#'         or `NA` if `obj` is not a code `tigris` object
#' @export
tigris_type <- function(obj) {
  if (is_tigris(obj)) return(attr(obj, "tigris"))
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

  state <- validate_state(state, allow_null = FALSE, .msg = FALSE)

  vals <- fips_codes[fips_codes$state_code == state, c("county", "county_code")]
  vals$county <- gsub("\ County$", "", vals$county)
  rownames(vals) <- NULL
  return(vals)

}

#' Row-bind `tigris` Spatial objects
#'
#' If multiple school district types are rbound, coerces to "sdall" and does it
#'
#' @param ... individual (optionally names) `tigris` Spatial objects or a list of them
#' @inheritParams rlang::args_error_context
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

rbind_tigris <- function(..., call = caller_env()) {

  elements <- list(...)

  if ((length(elements) == 1) &
      inherits(elements, "list")) {
    elements <- unlist(elements, recursive = FALSE) # Necessary given structure of sf objects
  }

  obj_classes <- unique(sapply(elements, class))
  obj_attrs <- sapply(elements, attr, "tigris")
  obj_attrs_u <- unique(obj_attrs)

  if (any(c("SpatialGridDataFrame", "SpatialLinesDataFrame",
        "SpatialPixelsDataFrame", "SpatialPointsDataFrame",
        "SpatialPolygonsDataFrame") %in% obj_classes) &
      "sf" %in% obj_classes) {

    abort("Cannot combine sp and sf objects", call = call)

  }

  #handling for attempts to rbind disparate school districts

  if(all(obj_attrs %in% c("unsd", "elsd", "scsd"))) { # 3 school district types

    warn("Multiple school district tigris types. Coercing to \'sdall\'.")

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

  if (obj_classes[1] %in% c("SpatialGridDataFrame", "SpatialLinesDataFrame",
                         "SpatialPixelsDataFrame", "SpatialPointsDataFrame",
                         "SpatialPolygonsDataFrame")) {

    abort(
      c(
        "Spatial* classes are no longer supported in tigris as of version 2.0.",
        "i" = "You will need to install an earlier version of tigris with `remotes::install_version()`."
      ),
      call = call
    )

  } else if ("sf" %in% obj_classes) {

    crs <- unique(sapply(elements, function(x) {
      return(st_crs(x)$epsg)
    }))

    if (length(crs) > 1) {
      abort(
        "All objects must share a single coordinate reference system.",
        call = call
      )
    }

    if (!any(sapply(obj_attrs, is.null)) &
        length(obj_attrs_u)==1) {

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

      if ("LINESTRING" %in% geometries & "MULTILINESTRING" %in% geometries) {
        elements <- lapply(elements, function(x) {
          st_cast(x, "MULTILINESTRING")
        })
      }

      tmp <- Reduce(rbind, elements) # bind_rows not working atm

      # Re-assign the original CRS if missing
      if (is.na(st_crs(tmp)$proj4string)) {
        st_crs(tmp) <- crs
      }

      attr(tmp, "tigris") <- obj_attrs_u
      return(tmp)

    } else {
      abort("Objects must all be the same type of tigris object.", call = call)
    }

  }

}

#' Erase water area from an input polygon dataset
#'
#' This function 'erases' water area from an input polygon dataset (typically a
#' Census dataset). 'Erase' is defined in the traditional GIS sense as the
#' removal of areas in an input layer from an erase layer, returning the
#' modified input layer. A common use-case is to improve cartographic
#' representation of locations where US Census polygons include more water area
#' than desired (e.g. New York City, Seattle) or to support contiguity-based
#' spatial analyses that might otherwise incorrectly assume that polygons across
#' bodies of water are neighbors.
#'
#' The function works by identifying US counties that intersect the input
#' polygon layer, then requesting water polygons (using `tigris::area_water()`)
#' to be erased from those input polygons. The `area_threshold` parameter can be
#' tuned to determine the percentile ranking of bodies of water (by area) to
#' use; the default is a percentile ranking of 0.75, erasing the largest 25
#' percent of water bodies in the region.
#'
#' Analysts will ideally have transformed the input coordinate reference system
#' (CRS) of their data to a projected CRS to improve performance; see
#' <https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html#coordinate-reference-systems>
#' for more information on how to perform CRS transformations.  Analysts should
#' also use this function with caution; the function may generate sliver
#' polygons or irregular geometries in the output layer, especially if the input
#' sf object was not obtained with the tigris package.  Also, the operation may
#' be quite slow for large input areas.
#'
#' @param input_sf An input sf object, ideally obtained with the tigris package
#'   or through tidycensus.
#' @param area_threshold The percentile rank cutoff of water areas to use in the
#'   erase operation, ranked by size. Defaults to 0.75, representing the water
#'   areas in the 75th percentile and up (the largest 25 percent of areas).
#'   This value may need to be modified by the user to achieve optimal results
#'   for a given location.
#' @param year The year to use for the water layer; defaults to 2022 unless the
#'   `tigris_year` option is otherwise set.
#' @inheritParams rlang::args_error_context
#' @return An output sf object representing the polygons in `input_sf` with
#'   water areas erased.
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
#'   st_transform(32148) %>%
#'   erase_water(area_threshold = 0.9)
#'
#' plot(king_erased$geometry)
#'
#' }
erase_water <- function(input_sf,
                        area_threshold = 0.75,
                        year = NULL,
                        arg = caller_arg(input_sf),
                        call = caller_env()) {
  if (!is_sf(input_sf)) {
    abort(
      "`{arg}` must be an sf object.",
      call = call
    )
  }

  year <- set_tigris_year(year, quiet = TRUE)

  filter_by <- prep_input_sfc(input_sf, arg = arg, call = call)

  # Grab a dataset of counties that overlap the input sf object quietly
  county_overlay <- counties(
    cb = TRUE,
    resolution = "500k",
    progress_bar = FALSE,
    year = year,
    filter_by = filter_by
  )

  # If nothing returned, exit
  if (nrow(county_overlay) == 0) {
    inform(
      "`input_sf` does not appear to intersect any United States counties.",
      "i" = "Returning unmodified `input_sf` input."
    )

    return(input_sf)
  }

  # Get a list of GEOIDs
  county_GEOIDs <- county_overlay$GEOID

  # Fetch water for those GEOIDs
  inform("Fetching area water data for `input_sf` location...")
  county_water <- lapply(
    county_GEOIDs,
    function(cty) {
      suppressMessages(area_water(
        state = substr(cty, 1, 2),
        county = substr(cty, 3, 5),
        progress_bar = FALSE,
        year = year,
        filter_by = filter_by
      ))
    }
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(dplyr::percent_rank(AWATER) >= area_threshold)

  if (nrow(county_water) == 0) {
    inform(
      c(
        "No overlapping water area found.",
        "i" = "Returning unmodified `input_sf`"
      )
    )
    return(input_sf)
  }

  inform(
    c(
      "Erasing water area..",
      "If this is slow, try a larger area threshold value."
    )
  )

  erased_sf <- suppressMessages(st_erase(input_sf, county_water))

  return(erased_sf)
}

#' Erase x geometry intersecting with y using [sf::st_difference()]
#' @noRd
st_erase <- function(x, y, ...) {
  if (!all(sf::st_is_valid(x))) {
    x <- sf::st_make_valid(x)
  }

  if (!all(sf::st_is_valid(y))) {
    y <- sf::st_make_valid(y)
  }

  x_crs <- sf::st_crs(x)

  if (sf::st_crs(y) != sf::st_crs(x)) {
    y <- sf::st_transform(y, crs = x_crs)
  }

  suppressWarnings(sf::st_difference(x, sf::st_union(y), ...))
}

#' Is object a `sf` object?
#'
#' @param x Object to check.
#' @noRd
is_sf <- function(x) {
  inherits(x, "sf")
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
#' @param year the data year; defaults to 2022
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
#' @name load_tiger_doc_template
NULL
