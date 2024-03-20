#' Retrieve GEOID from the Census Geocoder by address
#'
#' Returns GEOID for 2020 geographies.
#'
#' @param address A tibble/data frame with (at a minimum, others can be present)
#'   either character columns street, city, and state OR numeric columns lat and
#'   lon. Lat/lon columns take priority.
#' @param geoid_type GEOID level to return, `c('county', 'tract', 'block group', 'block')`.
#'   Defaults to block.
#' @return the original tibble with GEOIDs appended as a new column called
#'   `geoid`.
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
#' importFrom utils URLencode
#' importFrom httr GET stop_for_status
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
        inform("'zip' (", paste0(zip), ") was not a 5-character-long string composed of :digits:. Using only street, city, state.")
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
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
  if (length(response$result$addressMatches) == 0) {
    inform(paste0("Address (",
                   street, " ", city, " ", state,
                   ") returned no address matches. An NA was returned."))
    return(NA_character_)
  } else {
    if (length(response$result$addressMatches) > 1) {
      inform(paste0("Address (",
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
#' @param benchmark time period when a snapshot of address ranges was taken. As
#'   of early 2024, supported values include "Public_AR_Current",
#'   "Public_AR_Census2020", "Public_AR_ACS2023".
#' @param vintage census or survey that the address range relates to. See the
#'   [Find Geographic Coordinates
#'   form](https://geocoding.geo.census.gov/geocoder/geographies/coordinates)
#'   for supported values for each benchmark.
#'
#' @return A character string representing the Census block of the supplied
#'   lat/lon.
#'
#' @importFrom utils URLencode
#' @importFrom httr GET stop_for_status
#'
#' @author Josie Kressner, \email{josie@@transportfoundry.com}
#' @author Mark Richards, \email{Mark.Richards.002@@gmail.com}
#' @export
#'
call_geolocator_latlon <- function(lat, lon, benchmark = NULL, vintage = NULL) {
  # Build url
  url_full <- build_geolocator_laton_url(lat, lon, benchmark, vintage)

  # Check response
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
  if (length(response$result$geographies$`2020 Census Blocks`[[1]]$GEOID) == 0) {
    inform(paste0("Lat/lon (", lat, ", ", lon,
                   ") returned no geocodes. An NA was returned."))
    return(NA_character_)
  } else {

  #regex search for block group geography in response
  response_block<-grep(response[["result"]][["geographies"]], pattern = ".Block.")

  #check If a block group result is found or return NA
  #If block group response is found check GEOID length and return either NA for missing data or the value
  if(length(response_block) == 0){
    return(NA_character_)
  } else {
    if (length(response[["result"]][["geographies"]][[response_block]][[1]]$GEOID) == 0) {
      inform(paste0("Lat/lon (", lat, ", ", lon,
                     ") returned no geocodes. An NA was returned."))
      return(NA_character_)
    } else {
      if (length(response[["result"]][["geographies"]][[response_block]][[1]]$GEOID) > 1) {
        inform(paste0("Lat/lon (", lat, ", ", lon,
                       ") returned more than geocode. The first match was returned."))
      }
      return(response[["result"]][["geographies"]][[response_block]][[1]]$GEOID)
    }
  }

  }
}

#' @noRd
build_geolocator_laton_url <- function(
    lat,
    lon,
    benchmark = NULL,
    vintage = NULL,
    base_url = "https://geocoding.geo.census.gov/geocoder/geographies/coordinates",
    call = caller_env()
    ) {
  benchmark <- benchmark %||% "Public_AR_Current"
  benchmark <- set_geolocator_benchmark(benchmark, call = call)

  vintage <- vintage %||% "Current_Current"
  vintage <- set_geolocator_vintage(vintage, benchmark, call = call)

  paste0(
    base_url, "?",
    "x=", lon,"&y=", lat,
    "&benchmark=", benchmark,
    "&vintage=", vintage, "&format=json"
  )
}

#' @noRd
set_geolocator_benchmark <- function(
    benchmark,
    values = c("Public_AR_Current", "Public_AR_Census2020", "Public_AR_ACS2023"),
    call = caller_env()
) {

  if (!(benchmark %in% values)) {
    resp <- httr::GET("https://geocoding.geo.census.gov/geocoder/benchmarks")
    values <- httr::content(resp)
    values <- as.character(sapply(values[["benchmarks"]], `[`, "benchmarkName"))
  }

  arg_match(benchmark, values, error_call = call)
}

#' See form for possible values:
#' https://geocoding.geo.census.gov/geocoder/geographies/coordinates?form
#'
#' @noRd
set_geolocator_vintage <- function(
    vintage,
    benchmark,
    call = caller_env()) {
  if (benchmark == "Public_AR_Census2020") {
    prefix <- c("Census2010", "Census2020")
    suffix <- "Census2020"
  } else {
    prefix <- c(
      "Current",
      "Census2010",
      "Census2020",
      paste0("ACS", 2017:2023)
    )

    suffix <- switch (
      benchmark,
      "Public_AR_Current" = "Current",
      "Public_AR_ACS2023" = "ACS2023"
    )
  }

  arg_match(vintage, paste0(prefix, "_", suffix), error_call = call)
}
