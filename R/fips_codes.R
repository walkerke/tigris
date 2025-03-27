#' Dataset with FIPS codes for US states and counties
#'
#' Built-in dataset for use with the `lookup_code` function.
#' To access the data directly, issue the command `data(fips_codes)`.
#'
#' \itemize{
#'   \item `county`: County name, title-case
#'   \item `county_code`: County code. (3-digit, 0-padded, character)
#'   \item `state`: Upper-case abbreviation of state
#'   \item `state_code`: State FIPS code (2-digit, 0-padded, character)
#'   \item `state_name`: Title-case name of state
#' }
#'
#' @docType data
#' @keywords datasets
#' @name fips_codes
#'
#' @usage data(fips_codes)
#' @note Last updated 2023-05-18
#' @format A data frame with 3,256 rows and 5 variables
"fips_codes"
