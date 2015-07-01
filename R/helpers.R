#' Helper function to download Census data
#'
#' @import rgdal

load_tiger <- function(url) {

  tmp <- tempdir()

  file <- basename(url)

  download.file(url, file, mode = 'wb')

  unzip(file, exdir = tmp)

  shape <- gsub(".zip", "", file)

  data <- readOGR(dsn = tmp, layer = shape, encoding = "UTF-8", verbose = FALSE, stringsAsFactors = FALSE)

}


#'  Easily merge a data frame to a spatial data frame
#'
#'  The pages of StackOverflow are littered with questions about how to merge a regular data frame to a
#'  spatial data frame in R.  The \code{merge} function from the sp package operates under a strict set of
#'  assumptions, which if violated will break your data.  This function wraps a couple StackOverflow answers
#'  I've seen that work in a friendlier syntax.
#' @param spatial_data A spatial data frame to which you want to merge data.
#' @param data_frame A regular data frame that you want to merge to your spatial data.
#' @param by_sp The column name you'll use for the merge from your spatial data frame.
#' @param by_df The column name you'll use for the merge from your regular data frame.
#' @export

geo_join <- function(spatial_data, data_frame, by_sp, by_df) {

  spatial_data@data <- data.frame(spatial_data@data,
                                  data_frame[match(spatial_data@data[[by_sp]],
                                                   data_frame[[by_df]]), ])

  spatial_data


}


#' Look up state and county codes
#'
#' Function to look up the FIPS codes for states and optionally counties you'd like to load data for.  As the package functions require the codes to return the data correctly, this function makes it easy to find the codes that you need.
#'
#' @param state String representing the state you'd like to look up.  Accepts state names (spelled correctly), e.g. "Texas", or postal codes, e.g. "TX".
#' @param county The name of the county you'll like to search for.  The state that the county is located in must be supplied for this to work, as there are multiple counties with the same names across states.
#'
#' @export

lookup_code <- function(state, county = NULL) {

  if (!is.null(county)) {

    if (nchar(state) == 2) {

      sub <- fips_codes[fips_codes$state == state, ]

    } else {

      sub <- fips_codes[fips_codes$state_name == state, ]

    }

    index <- grep(county, sub$county)

    cty <- sub$county_code[index]

    cty_name <- sub$county[index]

    out <- paste0("The code for ", unique(sub$state_name),
                  " is '", unique(sub$state_code), "'", " and the code for ",
                  cty_name, " is '", cty, "'.")

    return(out)

  } else {

    state_index <- which(fips_codes$state == state)[1]

    st_code <- fips_codes$state_code[state_index]

    st_name <- fips_codes$state_name[state_index]

    out <- paste0("The code for ", st_name, " is '", st_code, "'.")

    return(out)

  }

}
