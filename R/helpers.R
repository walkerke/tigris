# Helper function to download Census data
#
# uses a global option "tigris_refresh" to control re-download of shapefiles (def: FALSE)
# also uses global option "tigris_use_cache" to determine whether new data files will
# be cached or not. (def: TRUE)
#
# Year currently defaults to 2015; give option to download other years


load_tiger <- function(url,
                       refresh=getOption("tigris_refresh", FALSE),
                       tigris_type=NULL,
                       year = getOption("tigris_year", 2015)) {

  if (year != 2015) {

    url <- gsub("2015", as.character(year), url)

    if (year < 2014) {

      url <- gsub("shp/", "", url) # A little dirty but should work for now for CB files prior to 2014

    }

  }

  use_cache <- getOption("tigris_use_cache", TRUE)
  tiger_file <- basename(url)

  obj <- NULL

  if (use_cache) {

    cache_dir <- user_cache_dir("tigris")
    if (!file.exists(cache_dir)) {
      dir.create(cache_dir, recursive=TRUE)
    }

    if (file.exists(cache_dir)) {

      file_loc <- file.path(cache_dir, tiger_file)

      if (refresh | !file.exists(file_loc)) {
        try(GET(url,
                write_disk(file_loc, overwrite=refresh),
                progress(type="down")), silent=TRUE)
      }

      shape <- gsub(".zip", "", tiger_file)

      if (refresh | !file.exists(file.path(user_cache_dir("tigris"),
                                 sprintf("%s.shp", shape)))) {
        unzip(file_loc, exdir = cache_dir, overwrite=TRUE)
      }

      obj <- readOGR(dsn = cache_dir, layer = shape, encoding = "UTF-8",
                     verbose = FALSE, stringsAsFactors = FALSE)

    }

  } else {

    tmp <- tempdir()
    download.file(url, tiger_file, mode = 'wb')
    unzip(tiger_file, exdir = tmp)
    shape <- gsub(".zip", "", tiger_file)

    obj <- readOGR(dsn = tmp, layer = shape, encoding = "UTF-8",
                   verbose = FALSE, stringsAsFactors = FALSE)

  }

  attr(obj, "tigris") <- "tigris"

  # this will help identify the object "sub type"
  if (!is.null(tigris_type)) attr(obj, "tigris") <- tigris_type

  return(obj)

}

#' Easily merge a data frame to a spatial data frame
#'
#' The pages of StackOverflow are littered with questions about how to merge a regular data frame to a
#' spatial data frame in R.  The \code{merge} function from the sp package operates under a strict set of
#' assumptions, which if violated will break your data.  This function wraps a couple StackOverflow answers
#' I've seen that work in a friendlier syntax.
#'
#' @param spatial_data A spatial data frame to which you want to merge data.
#' @param data_frame A regular data frame that you want to merge to your spatial data.
#' @param by_sp The column name you'll use for the merge from your spatial data frame.
#' @param by_df The column name you'll use for the merge from your regular data frame.
#' @param by (optional) If a named argument is supplied to the by parameter, geo_join will assume that the join columns in the spatial data and data frame share the same name.
#' @param how The type of join you'd like to perform.  The default, 'left', keeps all rows in the spatial data frame, and returns NA for unmatched rows.  The alternative, 'inner', retains only those rows in the spatial data frame that match rows from the target data frame.
#' @return a \code{SpatialXxxDataFrame} object
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
geo_join <- function(spatial_data, data_frame, by_sp, by_df, by = NULL, how = 'left') {

  if (!is.null(by)) {
    by_sp <- by
    by_df <- by
  }

  spatial_data@data <- data.frame(spatial_data@data,
                                  data_frame[match(spatial_data@data[[by_sp]],
                                                   data_frame[[by_df]]), ])

  if (how == 'inner') {

    matches <- match(spatial_data@data[[by_sp]], data_frame[[by_df]])

    spatial_data <- spatial_data[!is.na(matches), ]

    return(spatial_data)

  } else if (how == 'left') {

    return(spatial_data)

  } else {

    stop("The available options for `how` are 'left' and 'inner'.", call. = FALSE)

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

  state <- validate_state(state, .msg=FALSE)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

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

#' Returns \code{TRUE} if \code{obj} has a \code{tigris} attribute
#'
#' It's unlikely that said object was not created by this package
#'
#' @param obj R object to test
#' @return \code{TRUE} if \code{obj} was made by this package
#' @export
is_tigris <- function(obj) { !is.null(attr(obj, "tigris")) }

#' Get the type of \code{tigris} object \code{obj} is
#'
#' @param obj R object to test
#' @return character vector containing the \code{tigris} type of \code{obj}
#'         or \code{NA} if \code{obj} is not a code \code{tigris} object
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

  state <- validate_state(state, .msg=FALSE)

  if (is.null(state)) stop("Invalid state", call.=FALSE)

  vals <- fips_codes[fips_codes$state_code == state, c("county", "county_code")]
  vals$county <- gsub("\ County$", "", vals$county)
  rownames(vals) <- NULL
  return(vals)

}

#' Row-bind \code{tigris} Spatial objects
#'
#' @param ... individual (optionally names) \code{tigris} Spatial objects or a list of them
#' @return one combined Spatial object
#' @export
#' @examples \dontrun{
#' library(sp)
#' library(rgeos)
#' library(maptools)
#' library(maps)
#' library(tigris)
#'
#' me_ctys <- list_counties("me")
#' aw <- lapply(me_ctys$county_code[1:3], function(x) {
#'   area_water("Maine", x)
#' })
#' tmp <- rbind_tigris(aw)
#' tmp_simp <- gSimplify(tmp, tol=1/200, topologyPreserve=TRUE)
#' tmp_simp <- SpatialPolygonsDataFrame(tmp_simp, tmp@@data)
#' plot(tmp_simp)
#' }

rbind_tigris <- function(...) {

  elements <- list(...)

  if ((length(elements) == 1) &
      inherits(elements, "list")) {
    elements <- unlist(elements)
  }

  obj_classes <- unique(sapply(elements, class))
  obj_attrs <- sapply(elements, attr, "tigris")
  obj_attrs_u <- unique(obj_attrs)

  # all same type
  # all valid Spatial* type
  # none are from outside tigris
  # all same tigris "type"

  if (length(obj_classes) == 1 &
      obj_classes %in% c("SpatialGridDataFrame", "SpatialLinesDataFrame",
                         "SpatialPixelsDataFrame", "SpatialPointsDataFrame",
                         "SpatialPolygonsDataFrame") &
      !any(sapply(obj_attrs, is.null)) &
      length(obj_attrs_u)==1) {

    el_nams <- names(elements)

    if (is.null(el_nams)) {
      el_nams <- rep(NA, length(elements))
    }

    el_nams <- ifelse(el_nams == "", NA, el_nams)

    el_nams <- sapply(el_nams, function(x) {
       ifelse(is.na(x), gsub("-", "", UUIDgenerate(), fixed=TRUE), x)
    })

    tmp <- lapply(1:(length(elements)), function(i) {
      elem <- elements[[i]]
      spChFIDs(elem, sprintf("%s%s", el_nams[i], rownames(elem@data)))
    })

    tmp <- Reduce(spRbind, tmp)
    attr(tmp, "tigris") <- obj_attrs_u
    return(tmp)

  } else {
    stop("Objects must all be Spatial*DataFrame objects and all the same type of tigris object.", call.=FALSE)
  }

}
