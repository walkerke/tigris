#' Dataset with FIPS codes for US states and counties
#'
#' Built-in dataset for use with the \code{lookup_code} function.
#' To access the data directly, issue the command \code{data(fips_codes)}.

#' @title Dataset with FIPS codes for US states and counties
#' @description Built-in dataset for use with the \code{lookup_code} function.
#'              To access the data directly, issue the command \code{data(fips_codes)}.
#'
#' \itemize{
#'   \item \code{county}: County name, title-case
#'   \item \code{county_code}: County code. (3-digit, 0-padded, character)
#'   \item \code{state}: Upper-case abbreviation of state
#'   \item \code{state_code}: State FIPS code (2-digit, 0-padded, character)
#'   \item \code{state_name}: Title-case name of state
#' }
#'
#' @docType data
#' @keywords datasets
#' @name fips_codes
#'
#' @usage data(fips_codes)
#' @note Last updated 2015-07-01
#' @format A data frame with 3,236 rows and 5 variables
"fips_codes"
