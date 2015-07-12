# Helper function to download Census data
load_tiger <- function(url) {

  tmp <- tempdir()

  file <- basename(url)

  download.file(url, file, mode = 'wb')

  unzip(file, exdir = tmp)

  shape <- gsub(".zip", "", file)

  return(readOGR(dsn = tmp, layer = shape, encoding = "UTF-8",
                 verbose = FALSE, stringsAsFactors = FALSE))

  # was going to write some cleanup code here but will wait until I get
  # the cache feature working to do that.

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
#' @return a \code{SpatialXxxDataFrame} object
#' @export

geo_join <- function(spatial_data, data_frame, by_sp, by_df) {

  spatial_data@data <- data.frame(spatial_data@data,
                                  data_frame[match(spatial_data@data[[by_sp]],
                                                   data_frame[[by_df]]), ])

  return(spatial_data)


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
