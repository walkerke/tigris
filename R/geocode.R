#' Retrieve GEOID from the Census Geocoder by address
#'
#' Returns GEOID for 2010 geographies.
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

  call_end <- "&benchmark=Public_AR_Census2010&vintage=Census2010_Census2010&layers=14&format=json"

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


#' Call geolocator for one address with lat/lon, adds option to set benchmark and vintage if not provided it will default to the most recent.
#'
#' @param lat A numeric value between +-90
#' @param lon A numeric value between +-180
#' @param benchmark This is the version of the Census bureaus master address file that you would like to use to search your addresses. This function defaults to the most current version but allows for searching others. The full list of available benchmarks can be found here \href{https://geocoding.geo.census.gov/geocoder/benchmarks}{Census Benchmarks}
#' @param vintage This is the version of geographies inside the benchmark that you would like to use to search your addresses. This function defaults to the most current version but allows for searching others. The full list of vintages can eb found here \href{https://geocoding.geo.census.gov/geocoder/vintages?form}{Census Vintages}
#' @param Geographic_unit Indicateds what geogrpahic level you would like the resulting Federal Information Processing Standards (FIPS) code for. 
#' Must be one of the following options State, County, Tract, or Block
#'
#' @return A character string representing the Federal Information Processing Standards (FIPS) code that identifes the Census area for the supplied coordiantes and geogrpahic unit.
#'
#' @importFrom utils URLencode
#' @importFrom httr GET stop_for_status
#'
#' @author Josie Kressner, \email{josie@@transportfoundry.com}
#' @author Mark Richards, \email{Mark.Richards.002@@gmail.com}
#' @export
#'
call_geolocator_latlon <- function(lat,
                                   lon,
                                   benchmark="Public_AR_Current", 
                                   vintage="Current_Current", 
                                   Geographic_unit="Block") {
  if(Geographic_unit %in% c('State', 'County', 'Tract', 'Block')){
  } else {
    message(paste0("Geographic Unit requested is not State, County, Tract, or Block"))
    return(NA_character_)
  }
  
  #Combine elements into request URL
  Base_url <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates?"
  
  coordinates0 <- paste0("x=", lon,"&y=", lat) #API has swapped these assignments more than once (as of 3/2/2022 Longitude=X, Latitude=Y)
  benchmark0 <- paste0("&benchmark=", benchmark)
  vintage0 <- paste0("&vintage=", vintage, "&format=json")
  url_full <- paste0(Base_url, coordinates0, benchmark0, vintage0)
  
  
  #Make call to api
  r <- httr::GET(url_full)
  httr::stop_for_status(r, task = "complete API call: Check that Coordiantes are not out of bounds, and if used that the benchmark and vintage exist.") #converts http errors into warnings to show user
  response <- httr::content(r)
  
  #regex search for Geographic_unit in response
  response_block<-grep(response[["result"]][["geographies"]], pattern = paste0('.',Geographic_unit,'.'))
  #print(names(response[["result"]][["geographies"]]))
  
  #check If Geographic_unit is found in the results or return NA 
  #If Geographic_unit response is found check GEOID length and return either NA for missing data or the value
  if(length(response_block) == 0){
    message(paste("The selected geographic unit may not available for this combination of Benchmark, and Vintage. 
                  To better unserstand how to select a Benchamrk and Vintage see the technical documentation at 
                  https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/census-geocoder.html"))
    return(NA_character_)
  } else {
    if (length(response[["result"]][["geographies"]][[response_block]][[1]]$GEOID) == 0) {
      message(paste0("Lat/lon (", lat, ", ", lon,
                     ") returned no geocodes. An NA was returned."))
      return(NA_character_)
    } else {
      if (length(response[["result"]][["geographies"]][[response_block]][[1]]$GEOID) > 1) {
        message(paste0("Lat/lon (", lat, ", ", lon,
                       ") returned more than geocode. The first match was returned."))
      }
      return(response[["result"]][["geographies"]][[response_block]][[1]]$GEOID)
    }
  }
  
}


#' Batch Geocoder for the Census API
#' 
#' Takes a dataframe of address data (street, city, state, zip) and breaks it into batches that are sent to the Census bureaus batch geocoding API. It returns the provided data frame with geocoding results attached as additional columns. 
#' 
#' @param data Is the dataframe of address data. Should have columns that contain street number and name, City name, State, and ZIP code
#' @param Street This is the name of the column inside the provided dataframe that contains the street name and number. This should be given in quotes. Ex. "addr1"
#' @param City This is the name of the column inside the provided dataframe that contains the City name. This should be given in quotes. Ex. "City"
#' @param State This is the name of the column inside the provided dataframe that contains the State name. This should be given in quotes. Ex. "State"
#' @param ZIP This is the name of the column inside the provided dataframe that contains the ZIP code number. This should be given in quotes. Ex. "Zip5"
#' @param benchmark This is the version of the Census bureaus master address file that you would like to use to search your addresses. This function defaults to the most current version but allows for searching others. The full list of available benchmarks can be found here \href{https://geocoding.geo.census.gov/geocoder/benchmarks}{Census Benchmarks}
#' @param vintage This is the version of geographies inside the benchmark that you would like to use to search your addresses. This function defaults to the most current version but allows for searching others. The full list of vintages can eb found here \href{https://geocoding.geo.census.gov/geocoder/vintages?form}{Census Vintages}
#' @param batch_size This determines how many records will be included in each batch. This has implications for memory usage as the splitting of your dataframe is turned into a list that then gets sent one at a time to the API. Another consideration is that setting this value too high may cause timeouts while waiting for the API to process your request. The API limits this to a maximum of 10,000 records in a single batch. The function defaults to 1,000 in a single batch
#' 
#' @details This function will take the provided dataframe and format it so that it can be processed by the Census batch Geocoding API. The API requires a very specific format of Unique ID value, Street Address, City, State, Zip with no column headers. To achieve this an index value is attached to each record and a duplicate of the dataframe is made. Any extra data is removed from the duplicate dataframe and it is split into batches that are held in a list. The function then writes a batch to a temprary CSV file that it submits to the API. It then saves the results and moves on to the next batch in the list. When all of the batches have been sent and all results have been recieved the function combines the batch results into one large dataframe that it then joins to the originally provided data by matching the index values. This means that with large datasets this function may become memory intensive. Limiting the amount of additional data or the number of records in a dataframe when sending it to this function may provide a workaround to any memory constraints.
#' 
#' @return The returned dataframe will contain five additional columns
#' \itemize{
#' \item{\strong{Match} - }{A column indicating if a match was found}
#' \item{\strong{Match_type} - }{A column indicating if the match was an exact or appromiate match}
#' \item{\strong{Match_addr} - }{A column showing the address that the API was able to match to}
#' \item{\strong{Lon} - }{A column showing the longitude of a matched address}
#' \item{\strong{Lat} - }{A column showing the latitude of a matched address}
#' }
#' 
#' @references The web interface for the API can be found here:
#' \href{https://geocoding.geo.census.gov/geocoder/locations/addressbatch?form}{Census Batch Geolocator}
#'
#' @seealso Here you can read the API documentation: \href{https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf}{Census API Documentation}
#'
#'         
#' @examples #Generate some test data
#' Test_Batch<-data.frame(uid=c(1,2),
#'                       Street=c('4600 Silver Hill Road','400 15th St SE'),
#'                       City=c('Washington','Washington'),
#'                       State=c('DC','DC'),
#'                       ZIP=c(20233,20003))
#'
#' #call the function with the test data
#' call_geolocator_batch(data=Test_Batch,
#'                      Street = 'Street',
#'                      City = 'City',
#'                      State = 'State',
#'                      ZIP = 'ZIP',
#'                      benchmark = 'Public_AR_Current',
#'                      vintage = 'Current_Current',
#'                      batch_size=10)
#'                      
#' #Expected output                     
#' \tabular{cccccccccc}{
#' \strong{Index} \tab \strong{Street} \tab \strong{City} \tab \strong{State} \tab \strong{ZIP} \tab \strong{Match} \tab \strong{Match_type} \tab \strong{Match_Addr} \tab \strong{lon} \tab \strong{lat} \cr
#' 1 \tab 4600 Silver Hill Road \tab Washington \tab DC \tab 20233 \tab Match \tab Exact \tab 4600 SILVER HILL RD, WASHINGTON, DC, 20233 \tab -76.92744 \tab 38.845985 \cr
#' 2 \tab 400 15th St SE \tab Washington \tab DC \tab 20003 \tab Match \tab Exact \tab 400 15TH ST SE, WASHINGTON, DC, 20003 \tab -76.98365 \tab 38.884
#' }
#'
#' 
#' @author Mark Richards, \email{Mark.Richards.002@gmail.com}
#' 
call_geolocator_batch <- function(data, Street, City, State, ZIP, benchmark="Public_AR_Current", vintage="Current_Current", batch_size=1000) {
  if(missing(data)) {
    return(message("Must Specify dataframe"))
  }
  if(missing(Street)) {
    return("Must specify 'Street' Column name in dataframe.")
  }
  if(missing(City)) {
    return("Must specify 'City' Column name in dataframe.")
  }
  if(missing(State)) {
    return("Must specify 'State' Column name in dataframe.")
  }
  if(missing(ZIP)) {
    return("Must specify 'ZIP code' Column name in dataframe.")
  }
  if(batch_size>10000) {
    batch_size<-10000
    message("The batch size upper limit is 10,000 entries at a time")
  }
  
  
  #Add unique ID for each row, required for API call
  data$Census_batch_UID <- seq.int(nrow(data))
  
  #Remove extra data before API call
  data2<-data[,c('Census_batch_UID',
                 Street,
                 City,
                 State,
                 ZIP)]
  
  #Split the data into batches
  data2<-split(data2, rep(1:ceiling(nrow(data2)/batch_size),each=batch_size)[1:nrow(data2)])
  
  #set up progress bar
  pb = txtProgressBar(min = 0, max = length(data2), initial = 0, style = 3) 
  setTxtProgressBar(pb,length(data2)*.01)
  
  #Create data frame to hold results
  returned<-data.frame()
  
  #Make API call for each list element
  for(i in names(data2)){
    
    #use write.table becuase write.csv does not properly remove column headers and the API call cant handle them
    write.table(data2[[i]], file = paste0(tempdir(), "/", "Census_batch.csv"), row.names = FALSE, col.names = FALSE, sep=',')
    #call the API
    a<-POST("https://geocoding.geo.census.gov/geocoder/locations/addressbatch ", body = list(
      addressFile = upload_file(paste0(tempdir(), "/", "Census_batch.csv")),
      benchmark=benchmark,
      vintage=vintage),
      returntype="geographies",
      write_disk(addr <- tempfile(fileext = ".csv")))
    
    #add error handling for no result
    if(a$status_code != 200){
      message("\nAPI call error. Please check that the address column names, benchmark, and vintage are set correctly")
      return(NA_character_)
    }
    
    #read in results
    tmp<-read.csv(addr, header = FALSE, fill = TRUE, col.names=c("Census_batch_UID",
                                                                 "Address",
                                                                 "Match",
                                                                 "Match_type",
                                                                 "Match_Addr",
                                                                 "LatLon",
                                                                 "x",
                                                                 "xx"))
    
    
    returned<-rbind(returned,tmp)
    
    #increase progrss bar
    setTxtProgressBar(pb,as.numeric(i))
  }
  rm(i, tmp, pb)
  
  #separate out LAT, LON and put in separate columns
  returned$split<-str_split(returned$LatLon, pattern = ",")
  for(i in 1:nrow(returned)){
    returned$lon[i]<-returned$split[i][[1]][1]
    returned$lat[i]<-returned$split[i][[1]][2]
  }
  
  #Drop extra data
  returned<-returned[,c("Census_batch_UID",
                        "Match",
                        "Match_type",
                        "Match_Addr",
                        "lon",
                        "lat")]
  
  #merge data back together
  data<-merge(data, returned, by='Census_batch_UID')
  
  #drop Function generated unique IDs
  data$Census_batch_UID<-NULL
  
  return(data)
}
