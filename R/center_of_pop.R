#' Download a Centers of Population shapefile into R.
#'
#' Description from the Census Bureau: "The concept of the center of population as used 
#' by the U.S. Census Bureau is that of a balance point. The center of population is 
#' the point at which an imaginary, weightless, rigid, and flat (no elevation effects) 
#' surface representation of the 50 states (or 48 conterminous states for calculations 
#' made prior to 1960) and the District of Columbia would balance if weights of identical 
#' size were placed on it so that each weight represented the location of one person." 
#' For more information, please see the link provided.
#'
#' @param geography One of 'state', 'county', 'tract', or 'blockgroup'
#' @param state The state for which to download data. For state population centers 
#' and for year 2000 tracts, only national files are available, so `state` should
#' not be provided.
#' @param year The year for which  to download data. Only decennial census years 
#' 2000, 2010, and 2020 are available. 
#' @seealso \url{https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.2000.html}
#' @export
#' @examples \dontrun{
#' library(tigris)
#' library(ggplot2)
#' library(sf)
#' 
#' ctrs <- popcenters('county', state = 'wa', year = 2020)
#' counties <- counties(state = 'wa', year = 2020)
#' 
#' ggplot() + geom_sf(data = counties, fill = 'grey') + geom_sf(data = ctrs, color = 'red')
#' }
popcenters <- function(geography = c('state', 'county', 'tract', 'blockgroup'), state = NULL, year){
    geography <- match.arg(geography)
    if(!(year %in% c(2000, 2010, 2020))){
        stop("Centers of population are only available for decennial censuses 2000-2020.")
    }
    state <- validate_state(state)
    if((geography == 'state' || (geography == 'tract' && year == 2000))){
        if(!is.null(state)){
            stop("State-specific files are not available, leave state as null to download the nation-wide data.")
        }
    }else if(is.null(state)){
        stop("Provide a state.")
    }
    
    state_fips <- state
    state_abb <- tolower(unique(fips_codes[fips_codes$state_code == state_fips, ]$state))
    url <- case_when(
        # 2000
        year == 2000 && geography == 'state'      ~ paste0('cenpop2000/statecenters.txt'),
        year == 2000 && geography == 'county'     ~ paste0('cenpop2000/county/cou_', state_fips, '_', state_abb, '.txt'),
        year == 2000 && geography == 'tract'      ~ paste0('cenpop2000/tract/tract_pop.txt'),
        year == 2000 && geography == 'blockgroup' ~ paste0('cenpop2000/blkgrp/bg_', state_fips, '_', state_abb, '.txt'),
        # 2010 
        year == 2010 && geography == 'state'      ~ paste0('cenpop2010/CenPop2010_Mean_ST.txt'),
        year == 2010 && geography == 'county'     ~ paste0('cenpop2010/county/CenPop2010_Mean_CO', state_fips, '.txt'),
        year == 2010 && geography == 'tract'      ~ paste0('cenpop2010/tract/CenPop2010_Mean_TR', state_fips, '.txt'),
        year == 2010 && geography == 'blockgroup' ~ paste0('cenpop2010/blkgrp/CenPop2010_Mean_BG', state_fips, '.txt'),
        # 2020
        year == 2020 && geography == 'state'      ~ paste0('cenpop2020/CenPop2020_Mean_ST.txt'),
        year == 2020 && geography == 'county'     ~ paste0('cenpop2020/county/CenPop2020_Mean_CO', state_fips, '.txt'),
        year == 2020 && geography == 'tract'      ~ paste0('cenpop2020/tract/CenPop2020_Mean_TR', state_fips, '.txt'),
        year == 2020 && geography == 'blockgroup' ~ paste0('cenpop2020/blkgrp/CenPop2020_Mean_BG', state_fips, '.txt')
    )
    url <- paste0('https://www2.census.gov/geo/docs/reference/', url)

    if(geography == 'state'){ 
        col.names <- c('STATEFP', 'STNAME', 'POPULATION', 'LATITUDE', 'LONGITUDE') 
        colClasses <- c('character', 'character', 'integer', 'numeric', 'numeric') 
    }else if(geography == 'county'){ 
        if(year == 2000){
            col.names <- c('STATEFP', 'COUNTYFP', 'COUNAME', 'POPULATION', 'LATITUDE', 'LONGITUDE')
            colClasses <- c('character', 'character', 'character', 'integer', 'numeric', 'numeric') 

        }else{
            col.names <- c('STATEFP', 'COUNTYFP', 'COUNAME', 'STNAME', 'POPULATION', 'LATITUDE', 'LONGITUDE')
            colClasses <- c('character', 'character', 'character', 'character', 'integer', 'numeric', 'numeric') 
        }
    }else if(geography == 'tract'){ 
        col.names <- c('STATEFP', 'COUNTYFP', 'TRACTCE', 'POPULATION', 'LATITUDE', 'LONGITUDE') 
        colClasses <- c('character', 'character', 'character', 'integer', 'numeric', 'numeric') 

    }else if(geography == 'blockgroup'){ 
        col.names <- c('STATEFP', 'COUNTYFP', 'TRACTCE', 'BLKGRPCE', 'POPULATION', 'LATITUDE', 'LONGITUDE')
        colClasses <- c('character', 'character', 'character', 'character', 'integer', 'numeric', 'numeric')
    }
    
    if(year == 2000){
        header <- FALSE
    }else{
        header <- TRUE
    }
    if(geography == 'state' && year == 2000){
        dat <- read.fwf(url, widths = c(5, 20, 10, 12, 12), skip = 4, col.names = col.names, colClasses = colClasses) 
    }else{
        dat <- read.csv(url, col.names = col.names, colClasses = colClasses, header = header, na.strings = c('', 'NA', '+.', '-.'))
    }
    dat <- dat %>% mutate(across(where(is.character), stringr::str_trim))
    st_as_sf(dat, coords = c('LONGITUDE', 'LATITUDE'), crs = 4267, na.fail = FALSE)
}
