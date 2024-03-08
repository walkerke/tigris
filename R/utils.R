# Called to check to see if "state" is a FIPS code, full name or abbreviation.
#
# returns NULL if input is NULL
# returns valid state FIPS code if input is even pseud-valid (i.e. single digit but w/in range)
# returns NULL if input is not a valid FIPS code and require_state = FALSE
# errors if input is not a valid FIPS code and require_state = TRUE
validate_state <- function(state,
                           allow_null = TRUE,
                           require_state = FALSE,
                           .msg = interactive(),
                           error_call = caller_env()) {

  if (is.null(state)) {
    if (allow_null && !require_state) return(NULL)
    abort("Invalid state", call = error_call)
  }

  state <- tolower(trimws(state)) # forgive white space

  if (grepl("^[[:digit:]]+$", state)) { # we prbly have FIPS

    state <- sprintf("%02d", as.numeric(state)) # forgive 1-digit FIPS codes

    if (state %in% fips_state_table$fips) {
      return(state)
    } else {
      # perhaps they passed in a county FIPS by accident so forgive that, too,
      # but warn the caller
      state_sub <- substr(state, 1, 2)
      if (state_sub %in% fips_state_table$fips) {
        inform(
          sprintf(
            "Using first two digits of %s - '%s' (%s) - for FIPS code.",
            state, state_sub,
            fips_state_table[fips_state_table$fips == state_sub, "name"])
          )
        return(state_sub)
      }
    }

  } else if (grepl("^[[:alpha:]]+", state)) { # we might have state abbrev or name

    if (nchar(state) == 2 & state %in% fips_state_table$abb) { # yay, an abbrev!

      if (.msg)
        inform(
          sprintf("Using FIPS code '%s' for state '%s'",
                  fips_state_table[fips_state_table$abb == state, "fips"],
                  toupper(state))
          )
      return(fips_state_table[fips_state_table$abb == state, "fips"])

    } else if (nchar(state) > 2 & state %in% fips_state_table$name) { # yay, a name!

      if (.msg) {
        inform(
          sprintf("Using FIPS code '%s' for state '%s'",
                  fips_state_table[fips_state_table$name == state, "fips"],
                  simpleCapSO(state))
        )
      }

      return(fips_state_table[fips_state_table$name == state, "fips"])
    }

  }

  msg <- sprintf(
    "'%s' is not a valid FIPS code or state name/abbreviation", state
  )

  if (require_state) {
    abort(msg, call = error_call)
  }

  warn(msg)

  return(invisible(NULL))
}

# Some work on a validate_county function
#
#
validate_county <- function(state,
                            county,
                            allow_null = TRUE,
                            require_county = FALSE,
                            .msg = interactive(),
                            error_call = caller_env()) {
  # Get the state of the county
  state <- validate_state(
    state,
    allow_null = allow_null,
    require_state = require_county,
    error_call = error_call
  )

  if (is.null(county)) {
    if (allow_null && !require_county) return(NULL)
    abort("Invalid county", call = error_call)
  }

  county_table <- fips_codes[fips_codes$state_code == state, ] # Get a df for the requested state to work with

  if (grepl("^[[:digit:]]+$", county)) { # probably a FIPS code

    county <- sprintf("%03d", as.numeric(county)) # in case they passed in 1 or 2 digit county codes

    if (county %in% county_table$county_code) {
      return(county)

    } else {

      msg <- sprintf("'%s' is not a valid FIPS code for counties in %s",
                     county, county_table$state_name[1])

      if (require_county) {
        abort(msg, call = error_call)
      }

      warn(msg)

      return(invisible(NULL))
    }

  } else if ((grepl("^[[:alpha:]]+", county))) { # should be a county name

      county_index <- grepl(sprintf("^%s", county), county_table$county, ignore.case = TRUE)

      matching_counties <- county_table$county[county_index] # Get the counties that match

      if (length(matching_counties) == 0) {

        msg <- sprintf(
          "'%s' is not a valid name for counties in %s",
          county, county_table$state_name[1]
        )

        if (require_county) {
          abort(msg, call = error_call)
        }

        warn(msg)

        return(invisible(NULL))
      } else if (length(matching_counties) == 1) {

        if (.msg)
          inform(sprintf(
            "Using FIPS code '%s' for '%s'",
            county_table[county_table$county == matching_counties, "county_code"],
            matching_counties
          ))

        return(county_table[county_table$county == matching_counties, "county_code"])

      } else if (length(matching_counties) > 1) {

        ctys <- format_vec(matching_counties)

        msg <- warn(
          paste0(
            "Your county string matches ", ctys,
            " Please refine your selection."
          )
        )

        if (require_county) {
          abort(msg, call = error_call)
        }

        warn(msg)

        return(invisible(NULL))
      }

  }

}


# Quick function to return formatted string for county codes

format_vec <- function(vec) {

  out <- paste0(vec, ', ')

  l <- length(out)

  out[l - 1] <- paste0(out[l - 1], 'and ')

  out[l] <- gsub(', ', '.', out[l])

  return(paste0(out, collapse = ''))

}

# Function from SO to do proper capitalization

simpleCapSO <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


# Function to convert input shape to WKT for filter_by param
input_to_wkt <- function(input, arg = caller_arg(input), call = caller_env()) {
  if (is.null(input)) {
    return(character(0))
  }

  if (!inherits(input, c("sf", "sfc", "bbox"))) {
    if (is.numeric(input) && (length(input) == 4)) {
      names(input) <- c("xmin", "ymin", "xmax", "ymax")
      input <- sf::st_bbox(input, crs = 4269)
    } else {
      abort(
        c(paste0("Invalid input `", arg, "`"),
          "i" = "Supply an sf, sfc, bbox, or a length-4 vector that can be converted to a bbox."
        ),
        call = call
      )
    }
  }

  if (!inherits(input, "sfc")) {
    input <- sf::st_as_sfc(input)
  }

  # Make NAD83 for coordinate alignment
  input <- sf::st_transform(input, 4269)

  # Convert to WKT
  sf::st_as_text(input)
}

#' Set default year and validate year for tigris function
#'
#' [set_tigris_year()] returns year as a character string.
#'
#' @param year Year to use for download.
#' @param default Default year to use if "tigris_year" option is not set.
#' @param min_year Minimum year. Varies by geography and data source.
#' @param quiet If `TRUE`, do not display message about the year when
#'   downloading data.
#' @inheritParams source
#' @noRd
set_tigris_year <- function(year = NULL,
                            default = 2022,
                            min_year = 2011,
                            max_year = 2023,
                            quiet = FALSE,
                            message = NULL,
                            call = caller_env()) {
  if (is.null(year)) {
    year <- getOption("tigris_year", default)

    if (!quiet) {
      inform(sprintf("Retrieving data for the year %s", year))
    }
  }

  check_tigris_year(
    year,
    min_year = min_year,
    max_year = max_year,
    message = message,
    call = call
  )

  as.integer(year)
}

#' Check if year is valid
#'
#' @noRd
check_tigris_year <- function(year,
                              min_year = 2011,
                              max_year = 2023,
                              error_year = NULL,
                              message = NULL,
                              call = caller_env()) {
  year <- as.integer(year)

  if (length(year) != 1 || nchar(year) != 4) {
    abort(
      "`year` must be a an integer or string with a single year.",
      call = call
    )
  }

  if (!is.null(error_year)) {
    if (year == error_year) {
      msg <- message %||% sprintf("`year` can't be %s", error_year)
      abort(msg, call = call)
    }

    return(invisible(NULL))
  }

  if ((year >= min_year) && year <= max_year) {
    return(invisible(NULL))
  }

  if (!is.null(message)) {
    abort(message, call = call)
  }

  msg <- "%s is not currently available for years prior to %s."
  limit_year <- min_year

  if (year > max_year) {
    msg <- "%s is not currently available for years after %s."
    limit_year <- max_year
  }

  trace <- trace_back()
  fname <- call_name(quote(trace[["call"]][[1]]))
  fname <- paste0("`", fname, "`")

  if (fname == "`[[`") {
    fname <- "Requested data"
  }

  msg <- sprintf(msg, fname, limit_year)

  abort(
    c(
      msg,
      "*" = "To request this feature, file an issue at https://github.com/walkerke/tigris/issues"
    ),
    call = call
  )
}


#' Check if resolution is valid
#'
#' @noRd
check_tigris_resolution <- function(resolution,
                                    values = c("500k", "5m", "20m"),
                                    ignore.case = TRUE,
                                    error_call = caller_env()) {
  if (ignore.case) {
    resolution <- tolower(resolution)
  }

  arg_match(resolution, values = values, error_call = error_call)
}

#' Set tigris URL
#'
#' [url_tiger()] returns a download URL for a tigris shapefile.
#'
#' @inheritParams sprintf
#' @noRd
url_tiger <- function(fmt,
                      ...,
                      base_url = "https://www2.census.gov/geo/tiger/",
                      ext = ".zip") {

  paste0(base_url, sprintf(fmt = fmt, ...), ext)

}

#' Remove "shp/" from a character vector
#'
#' @noRd
remove_shp <- function(x) {
  gsub("shp/", "", x)
}


#' Error if year matches error year
#'
#' @noRd
check_cb_year <- function(year = 1990, error_year = 1990, call = caller_env()) {
 check_tigris_year(
   year,
   error_year = error_year,
   message = sprintf("Please specify `cb = TRUE` to get %s data.", error_year),
   call = call)
}

#' Static version of `stringr::str_pad` based on `stringstatic::str_pad()`
#'
#' @noRd
pad_str <- function(
    string, width, side = c("left", "right", "both"), pad = " "
) {

  pad_width <- width - nchar(string, type = "width")
  pad_width[pad_width < 0] <- 0

  switch(
    side,
    "left" = paste0(strrep(pad, pad_width), string),
    "right" = paste0(string, strrep(pad, pad_width)),
    "both" = paste0(
      strrep(pad, floor(pad_width / 2)),
      string,
      strrep(pad, ceiling(pad_width / 2))
    )
  )
}
