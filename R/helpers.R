#' Helper function to download Census data
#'
#' @export
#' @import rgdal

load_tiger <- function(url) {

  tmp <- tempdir()

  file <- basename(url)

  download.file(url, file, mode = 'wb')

  unzip(file, exdir = tmp)

  shape <- gsub(".zip", "", file)

  data <- readOGR(dsn = tmp, layer = shape, encoding = "UTF-8", verbose = FALSE)

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



