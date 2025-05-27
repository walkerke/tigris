#' Check input argument
#' @noRd
check_tigris_arg <- function(
  arg,
  require_arg = FALSE,
  allow_null = TRUE,
  multiple = FALSE,
  error_arg = caller_arg(arg),
  error_call = caller_env()
) {
  if (is.null(arg)) {
    if (allow_null && !require_arg) {
      return(invisible(NULL))
    }

    cli_abort("{.arg {error_arg}} can't be `NULL`", call = error_call)
  }

  # Error if length is greater than 1
  if (!multiple && length(arg) > 1) {
    cli_abort(
      "{.arg {error_arg}} must be length 1, not length {length(arg)}",
      error_call = error_call
    )
  }
}

#' Is x only digits?
#' @noRd
is_digits <- function(x) {
  grepl("^[[:digit:]]+$", x)
}

#' Is x only alpha characters?
#' @noRd
is_alpha <- function(x) {
  grepl("^[[:alpha:]]+", x)
}

#' Pad a short FIPS code
#' @noRd
pad_fips <- function(string, width = 2) {
  pad_str(string, width = width, pad = "0")
}

#' Is x a state FIPS?
#' @noRd
is_stfips <- function(x) {
  is_digits(x) & (nchar(x) == 2) & (x %in% fips_state_table[["fips"]])
}

#' Is x a state name?
#' @noRd
is_stname <- function(x) {
  (nchar(x) > 2) & (tolower(x) %in% fips_state_table[["name"]])
}

#' Is x a state USPS abbreviation?
#' @noRd
is_stusps <- function(x) {
  is_alpha(x) & (nchar(x) == 2) & (tolower(x) %in% fips_state_table[["abb"]])
}

#' Match state name values
#' @noRd
match_state_name <- function(
  state,
  multiple = FALSE,
  require_state = FALSE,
  error_arg = caller_arg(state),
  error_call = caller_env()
) {
  state <- tolower(state)
  state_fips <- rep_along(state, NA_character_)

  is_state_abb <- is_stusps(state)

  if (any(is_state_abb)) {
    state_abb <- match(state, fips_state_table[["abb"]])
    state_abb <- state_abb[!is.na(state_abb)]
    state_fips[is_state_abb] <- fips_state_table[state_abb, "fips"]
  }

  is_state_name <- is_stname(state)

  if (any(is_state_name)) {
    state_names <- match(state, fips_state_table[["name"]])
    state_names <- state_names[!is.na(state_names)]
    state_fips[is_state_name] <- fips_state_table[state_names, "fips"]
  }

  if (!require_state) {
    return(state_fips)
  }

  arg_match(
    state_fips,
    multiple = multiple,
    values = fips_state_table[["fips"]],
    error_arg = error_arg,
    error_call = error_call
  )
}

#' Match state FIPS to valid values
#' @noRd
match_state_fips <- function(
  state,
  values = NULL,
  strict = FALSE,
  multiple = FALSE,
  require_state = FALSE,
  error_arg = "state",
  error_call = caller_env()
) {
  # forgive 1-digit FIPS codes or county FIPS codes
  if (!strict) {
    short_state <- nchar(state) < 2
    long_state <- nchar(state) > 2

    if (any(short_state)) {
      state[short_state] <- pad_fips(state[short_state], 2)
    }

    if (any(long_state)) {
      cli_inform(
        "Subsetting first two digits from supplied {.arg {error_arg}} to make valid FIPS codes: {state[long_state]}"
      )

      state[long_state] <- substr(state[long_state], 1, 2)
    }
  }

  values <- values %||% fips_state_table[["fips"]]
  valid_state <- state %in% values

  if (all(valid_state)) {
    return(state)
  }

  if (!require_state) {
    state[!valid_state] <- NA_character_
    return(state)
  }

  arg_match(
    state,
    multiple = multiple,
    values = values,
    error_arg = error_arg,
    error_call = error_call
  )
}

#' Check if state is a FIPS code, full name or abbreviation
#'
#' [validate_state()] checks if an input state values is a FIPS code, full name
#' or abbreviation. If some or all input values are invalid, error if
#' `require_state = TRUE`, return `NULL` if all inputs are invalid, or warn and
#' return valid input values.
#'
#' @param state State name, abbreviation, or FIPS code. Not case sensitive. If a
#'   single digits FIPS code or longer county FIPS code is passed, the values
#'   are padded with "0" to subset to allow use as a state value.
#' @param allow_null If `TRUE` (default), allow `NULL` input values by invisibly
#' returning `NULL`.
#' @param require_state If `FALSE` (default), warn instead of error on invalid
#' input values.
#' @param multiple If `FALSE` (default), require input to match a single state
#' or county. If `TRUE`, allow multiple values.
#' @returns
#' - `NULL` if input is `NULL` and `allow_null = TRUE` and `require_state = FALSE`
#' - valid state FIPS code if input is even pseudo-valid (i.e. single digit but w/in range)
#' - `NULL` if input is not a valid FIPS code and `require_state = FALSE`
#' - errors if input is not a valid FIPS code and `require_state = TRUE` or if `state` matched multiple values
#' @keywords internal
#' @export
validate_state <- function(
  state,
  allow_null = TRUE,
  require_state = FALSE,
  multiple = FALSE,
  .msg = is_interactive(),
  error_call = caller_env()
) {
  check_tigris_arg(
    state,
    allow_null = allow_null,
    require_arg = require_state,
    multiple = multiple,
    error_call = error_call
  )

  if (allow_null && is.null(state)) {
    return(invisible(NULL))
  }

  # forgive white space
  state <- trimws(state)
  state_fips <- rep_along(state, NA_character_)
  state_likely_fips <- is_digits(state)
  state_likely_name <- is_alpha(state)

  # Match state FIPS codes
  if (any(state_likely_fips)) {
    state_fips[state_likely_fips] <- match_state_fips(
      state[state_likely_fips],
      multiple = multiple,
      require_state = require_state,
      error_call = error_call
    )
  }

  # Match state names and abbreviations
  if (any(state_likely_name)) {
    state_fips[state_likely_name] <- match_state_name(
      state[state_likely_name],
      multiple = multiple,
      require_state = require_state,
      error_call = error_call
    )
  }

  state_is_fips <- is_stfips(state_fips)

  if (all(state_is_fips)) {
    return(state_fips)
  }

  if (!any(state_is_fips) && require_state) {
    cli_abort(
      "{.arg state} must contain a valid FIPS codes or state name/abbreviations.",
      call = error_call
    )
  }

  message <- "{.arg state} contains {cli::no(length(state[!state_is_fips]))} invalid FIPS code{?s} or state name/abbreviations: {.val {state[!state_is_fips]}}"

  cli_warn(message, call = error_call)

  if (length(state[state_is_fips]) == 0) {
    return(NULL)
  }

  state[state_is_fips]
}

#' Get a vector of county FIPS codes named with county names
#' @noRd
county_values <- function(
  state,
  ...,
  require_state = TRUE,
  multiple = FALSE,
  error_call = caller_env()
) {
  state <- validate_state(
    state,
    require_state = require_state,
    ...,
    multiple = multiple,
    error_call = error_call
  )

  if (!require_state && is.null(state)) {
    return(NULL)
  }

  state_counties <- tigris::fips_codes[
    tigris::fips_codes[["state_code"]] == state,
  ]
  set_names(state_counties[["county_code"]], state_counties[["county"]])
}

#' Match county FIPS values
#' @noRd
match_county_fips <- function(
  county,
  values = NULL,
  state = NULL,
  multiple = FALSE,
  require_county = FALSE,
  strict = FALSE,
  error_call = caller_env()
) {
  if (is.null(values) && !is.null(state)) {
    values <- county_values(state, error_call = error_call)
  }

  # forgive 1-digit FIPS codes or county FIPS codes
  if (!strict) {
    short_county <- nchar(county) < 3

    if (any(short_county)) {
      county[short_county] <- pad_fips(county[short_county], 3)
    }
  }

  valid_county <- county %in% values

  if (all(valid_county)) {
    return(county)
  }

  if (!require_county) {
    county[!valid_county] <- NA_character_
    return(county)
  }

  arg_match(
    county,
    values = values,
    multiple = multiple,
    error_call = error_call
  )
}

#' Match county name values
#' @noRd
match_county_name <- function(
  county,
  values = NULL,
  state = NULL,
  require_county = FALSE,
  multiple = FALSE,
  error_call = caller_env()
) {
  if (is.null(values) && !is.null(state)) {
    values <- county_values(state, error_call = error_call)
  }

  county_matches <- lapply(
    sprintf("^%s", county),
    \(p) {
      values[grepl(p, names(values), ignore.case = TRUE)]
    }
  )

  missing_matches <- vapply(county_matches, is_empty, logical(1))
  uncertain_matches <- vapply(
    county_matches,
    \(x) {
      length(x) > 1
    },
    logical(1)
  )

  message <- NULL

  if (!any(missing_matches) && !any(uncertain_matches)) {
    return(as.character(county_matches))
  }

  if (any(missing_matches)) {
    missing_county <- county[missing_matches]
    county_matches[missing_matches] <- NA_character_

    message <- c(
      message,
      "{.arg county} input can't be matched to any valid value: {missing_county}"
    )
  }

  if (any(uncertain_matches)) {
    uncertain_county <- county[uncertain_matches]
    county_matches[uncertain_matches] <- NA_character_

    message <- c(
      message,
      "{.arg county} input is ambigous and matches multiple values: {uncertain_county}"
    )
  }

  county_matches <- as.character(county_matches)

  if (require_county) {
    cli_abort(message, call = error_call)
  }

  cli_warn(message, call = error_call)

  county_matches
}

#' Check if county is a FIPS code, full name or abbreviation.
#'
#' [validate_county()] checks if an input `county` is a FIPS code, name or
#' abbreviation. If some or all input values are invalid, error if
#' `require_county = TRUE`, return `NULL` if all inputs are invalid, or warn and
#' return valid input values.
#'
#' @inheritParams validate_state
#' @param county County name, abbreviation, or FIPS code. Not case sensitive.
#'
#' @returns
#' - `NULL` if input is `NULL` and `allow_null = TRUE` and `require_county = FALSE`
#' - valid county FIPS code if input is even pseudo-valid (i.e. single digit but w/in range)
#' - `NULL` if input is not a valid FIPS code and `require_county = FALSE`
#' - errors if input is not a valid FIPS code and `require_county = TRUE` or if `county` matched multiple values and `multiple = FALSE`
#' @keywords internal
#' @export
validate_county <- function(
  state,
  county,
  allow_null = TRUE,
  require_county = FALSE,
  multiple = FALSE,
  .msg = is_interactive(),
  error_call = caller_env()
) {
  check_tigris_arg(
    county,
    allow_null = allow_null,
    require_arg = require_county,
    multiple = multiple,
    error_call = error_call
  )

  # Get the state of the county
  county_values <- county_values(
    state,
    allow_null = allow_null,
    .msg = .msg,
    require_state = require_county,
    multiple = FALSE,
    error_call = error_call
  )

  if (allow_null && is.null(county)) {
    return(invisible(NULL))
  }

  county <- trimws(county)
  county_fips <- rep_along(county, NA_character_)
  county_likely_fips <- is_digits(county)
  county_likely_name <- is_alpha(county)

  # Match county FIPS codes
  if (any(county_likely_fips)) {
    county_fips[county_likely_fips] <- match_county_fips(
      county = county[county_likely_fips],
      values = county_values,
      multiple = multiple,
      require_county = require_county,
      error_call = error_call
    )
  }

  # Match county names
  if (any(county_likely_name)) {
    county_fips[county_likely_name] <- match_county_name(
      county = county[county_likely_name],
      values = county_values,
      require_county = require_county,
      multiple = multiple,
      error_call = error_call
    )
  }

  # Flag missing county values
  na_county_fips <- is.na(county_fips)

  if (.msg) {
    message_counties <- paste0(
      "{.val ",
      county_fips[!na_county_fips],
      "} for {.val ",
      county[!na_county_fips],
      "}"
    )

    message <- c(
      "*" = "Using FIPS code{?s} {message_counties}"
    )

    if (any(na_county_fips)) {
      message <- c(
        message,
        "!" = "Dropping invalid {.arg county} value{?s}: {county[na_county_fips]}"
      )
    }

    cli_inform(message)
  }

  if (all(na_county_fips)) {
    return(NULL)
  }

  county_fips[!na_county_fips]
}

# Function from SO to do proper capitalization
#' @noRd
simpleCapSO <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
}

#' Static version of `stringr::str_pad` based on `stringstatic::str_pad()`
#'
#' @noRd
pad_str <- function(
  string,
  width,
  side = c("left", "right", "both"),
  pad = " "
) {
  pad_width <- width - nchar(string, type = "width")
  pad_width[pad_width < 0] <- 0

  side <- match.arg(side)

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


# Function to convert input shape to WKT for filter_by param
#' @noRd
input_to_wkt <- function(
  input,
  crs = 4269,
  geos_method = "valid_structure",
  arg = caller_arg(input),
  call = caller_env()
) {
  if (is.null(input)) {
    return(character(0))
  }

  input_sfc <- prep_input_sfc(
    input = input,
    crs = crs,
    geos_method = geos_method,
    arg = arg,
    call = call
  )

  # Convert to WKT
  sf::st_as_text(input_sfc)
}

#' Prep sfc from sf, sfc, or bbox input
#' @noRd
prep_input_sfc <- function(
  input,
  crs = 4269,
  geos_method = "valid_structure",
  arg = caller_arg(input),
  call = caller_env()
) {
  if (!inherits(input, "bbox") && is.numeric(input) && (length(input) == 4)) {
    # TODO: Check if input is already named
    names(input) <- c("xmin", "ymin", "xmax", "ymax")
    input <- sf::st_bbox(input, crs = crs)
  }

  if (!inherits(input, c("sf", "sfc", "bbox"))) {
    cli_abort(
      c(
        "Invalid input {.arg {arg}}",
        "i" = "Supply an sf, sfc, bbox, or a length-4 vector
          that can be converted to a bbox."
      ),
      call = call
    )
  }

  if (!inherits(input, "sfc")) {
    input <- sf::st_as_sfc(input)
  }

  if (!all(sf::st_is_valid(x))) {
    input <- sf::st_make_valid(input, geos_method = geos_method)
  }

  if (length(input) > 1) {
    cli_inform(
      c(
        "!" = "{.arg {arg}} contains multiple geometries and may not work as expected.",
        "i" = "Unioning .arg {arg}} geometries with {.fn sf::st_union}."
      )
    )

    input <- sf::st_union(input)
    input <- sf::st_make_valid(input, geos_method = geos_method)
  }

  # Make NAD83 for coordinate alignment
  sf::st_transform(input, crs = crs)
}
