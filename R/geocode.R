#' Retrieve GEOID from the Census Geocoder by address
#'
#' Returns GEOID for 2020 geographies.
#'
#' @param address A tibble/data frame with (at a minimum, others can be present)
#'   either character columns street, city, and state OR numeric columns lat and
#'   lon. Lat/lon columns take priority.
#' @param geoid_type GEOID level to return, \code{c('county', 'tract', 'block group', 'block')}.
#'   Defaults to block.
#' @return the original tibble with GEOIDs appended as a new column called
#'   \code{geoid}.
#'
#' @author Josie Kressner, \email{josie@@transportfoundry.com}
#'
#' @importFrom dplyr mutate
#' @export
#' @examples \dontrun{
#' airports <- dplyr::data_frame(
#'   street = "700 Catalina Dr", city = "Daytona Beach", state = "FL"
#' )
#' append_geoid(airports, 'tract')
#' }
append_geoid <- function(address, geoid_type = 'block') {

  if ("lat" %in% colnames(address) && "lon" %in% colnames(address)) {
    # Call for each row of the data
    geoids <- vector(mode="character", length = nrow(address))
    for (i in 1:nrow(address)) {
      geoids[i] <- call_geolocator_latlon(address$lat[i], address$lon[i])
    }
  } else {
    # If street, city, or state columns are factors, convert them
    # Call for each row of the data
    geoids <- vector(mode="character", length = nrow(address))
    for (i in 1:nrow(address)) {
      geoids[i] <- call_geolocator(
        as.character(address$street[i]),
        as.character(address$city[i]),
        as.character(address$state[i])
      )
    }
  }

  # Append onto database
  address <- dplyr::mutate(address, geoid = geoids)

  # AABBBCCCCCCDEEE
  if (geoid_type == 'county') {
    end <- 5
  } else if (geoid_type == 'tract') {
    end <- 11
  } else if (geoid_type == 'block group') {
    end <- 12
  } else {
    end <- 15
  }
  address <- dplyr::mutate(address,
                           geoid = ifelse(is.na(.data$geoid), NA_character_, substr(.data$geoid, 1, end)))

  return(address)
}


#' Call geolocator for one address
#'
#' @param street A character string indicating a street name and number
#' @param city A character string indicating a city
#' @param state A two-digit character string with a state postal code
#' @param zip A five-digit character string with a postal zip code. Optional parameter.
#'
#' @return A character string representing the Census block of the supplied
#'   address.
#'
#' @export
#'
call_geolocator <- function(street, city, state, zip = NA) {

  call_start <- "https://geocoding.geo.census.gov/geocoder/geographies/address?"

  if(is.na(zip)){
    # Build url when zip is default/NA
    url <- paste0(
      "street=", utils::URLencode(street),
      "&city=", utils::URLencode(city),
      "&state=", state
    )}

  if(!is.na(zip)){
    # Build url when zip is not default/NA
    if(inherits(zip, "character") & nchar(zip) == 5 & !grepl("\\D", zip)){
      url <- paste0(
        "street=", utils::URLencode(street),
        "&city=", utils::URLencode(city),
        "&state=", state,
        "&zip=", zip
      )} else {
        message("'zip' (", paste0(zip), ") was not a 5-character-long string composed of :digits:. Using only street, city, state.")
        url <- paste0(
          "street=", utils::URLencode(street),
          "&city=", utils::URLencode(city),
          "&state=", state
        )
      }
    }

  call_end <- "&benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&layers=10&format=json"

  url_full <- paste0(call_start, url, call_end)

  # Check response
  resp <- tigris_perform(tigris_request(url_full))
  stop_for_geocoder_status(resp)
  response <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (length(response$result$addressMatches) == 0) {
    message(paste0("Address (",
                   street, " ", city, " ", state,
                   ") returned no address matches. An NA was returned."))
    return(NA_character_)
  } else {
    if (length(response$result$addressMatches) > 1) {
      message(paste0("Address (",
                     street, " ", city, " ", state,
                     ") returned more than one address match. The first match was returned."))
    }
    return(response$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$GEOID)
  }
}


#' Call geolocator for one address with lat/lon, adds option to set benchmark and vintage if not provided it will default to the most recent.
#'
#' @param lat A numeric value
#' @param lon A numeric value
#' @param benchmark time period when a snapshot of address ranges was taken.
#'   Defaults to \code{"Public_AR_Current"}; see
#'   \url{https://geocoding.geo.census.gov/geocoder/benchmarks} for available benchmarks.
#' @param vintage census or survey that the address range relates to, e.g.
#'   \code{"Census2020_Current"}. Defaults to \code{"Current_Current"}; available
#'   vintages depend on the benchmark, see
#'   \url{https://geocoding.geo.census.gov/geocoder/vintages?benchmark=Public_AR_Current}.
#'   ACS vintages don't include blocks, so they return \code{NA}.
#'
#' @return A character string representing the Census block of the supplied
#'   lat/lon.
#'
#' @importFrom utils URLencode
#'
#' @author Josie Kressner, \email{josie@@transportfoundry.com}
#' @author Mark Richards, \email{Mark.Richards.002@@gmail.com}
#' @export
#'
call_geolocator_latlon <- function(lat, lon, benchmark, vintage) {
  if(missing(benchmark)) {
    benchmark<-"Public_AR_Current"
  } else {
    benchmark<-benchmark
  }
  if(missing(vintage)) {
    vintage<-"Current_Current"
  } else {
    vintage<-vintage
  }
  # Build url
  call_start <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates?"

  url <- paste0("x=", lon,"&y=", lat)

  benchmark0 <- paste0("&benchmark=", benchmark)
  vintage0 <- paste0("&vintage=", vintage, "&format=json")

  url_full <- paste0(call_start, url, benchmark0, vintage0)
  #print(url_full)
  # Check response
  resp <- tigris_perform(tigris_request(url_full))
  if (httr2::resp_status(resp) == 400) {
    cli_abort(
      c(
        "The Census geocoder rejected the request (HTTP 400); check that `benchmark` and `vintage` are valid.",
        "i" = "Benchmarks: {.url https://geocoding.geo.census.gov/geocoder/benchmarks}",
        "i" = "Vintages: {.url https://geocoding.geo.census.gov/geocoder/vintages?benchmark={benchmark}}"
      )
    )
  }
  stop_for_geocoder_status(resp)
  response <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  geographies <- response[["result"]][["geographies"]]

  # The block layer is "2020 Census Blocks" for Current_Current and "Census
  # Blocks" for the Census 2010 / 2020 vintages; ACS vintages have no blocks
  response_block <- grep("Census Blocks$", names(geographies))

  block_matches <- if (length(response_block) > 0) geographies[[response_block[1]]]
  geoid <- if (length(block_matches) > 0) block_matches[[1]]$GEOID

  if (length(geoid) == 0) {
    message(paste0("Lat/lon (", lat, ", ", lon,
                   ") returned no geocodes. An NA was returned."))
    return(NA_character_)
  }

  if (length(block_matches) > 1) {
    message(paste0("Lat/lon (", lat, ", ", lon,
                   ") returned more than one geocode. The first match was returned."))
  }

  return(geoid)
}

# Error for a geocoder response with an HTTP error status, in the wording
# httr's stop_for_status() used (e.g. "Bad Gateway (HTTP 502)."). Temporary
# server errors have already been retried, so those say so.
stop_for_geocoder_status <- function(resp) {
  status <- httr2::resp_status(resp)

  if (status < 400) {
    return(invisible(resp))
  }

  message <- sprintf("%s (HTTP %s).", httr2::resp_status_desc(resp), status)

  if (status %in% tigris_retry_statuses) {
    message <- c(
      message,
      "i" = sprintf(
        "The Census geocoder may be temporarily unavailable (tried %s times); please try again later.",
        tigris_max_tries
      )
    )
  }

  cli_abort(message, class = "tigris_geocoder_error", call = NULL)
}
