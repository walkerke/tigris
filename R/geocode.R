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
    if(class(zip) == "character" & nchar(zip) == 5 & !grepl("\\D", zip)){
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

  call_end <- "&benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&layers=14&format=json"

  url_full <- paste0(call_start, url, call_end)

  # Check response
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
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


#' Call geolocator for one address with lat/lon, adds option to set benchmark and vintage if not provided defualts to most recent.
#'
#' @param lat A numeric value
#' @param lon A numeric value
#' @param benchmark time period when a snapshot of address ranges was taken
#' @param vintage census or survey that the address range relates to
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
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
  if (length(response$result$geographies$`2020 Census Blocks`[[1]]$GEOID) == 0) {
    message(paste0("Lat/lon (", lat, ", ", lon,
                   ") returned no geocodes. An NA was returned."))
    return(NA_character_)
  } else {
    if (length(response$result$geographies$`2020 Census Blocks`[[1]]$GEOID) > 1) {
      message(paste0("Lat/lon (", lat, ", ", lon,
                     ") returned more than geocode. The first match was returned."))
    }
    return(response$result$geographies$`2020 Census Blocks`[[1]]$GEOID)
  }
}
