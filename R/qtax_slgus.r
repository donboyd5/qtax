#' National Quarterly State and Local Government Tax Revenue Data.
#'
#' Quarterly tax revenue data for state and local governments for the U.S. in aggregate, by tax type, from 1992 through most recent.
#'
#' @format Data frame with 1 row per tax, per quarter.
#' \describe{
#' \item{stabbr}{state abbreviation - "US" for all records}
#' \item{date}{first day of quarter}
#' \item{ic}{item code for the variable, as defined by Census Bureau}
#' \item{vname}{variable name (a memorable name for the item code, ic)}
#' \item{value}{amount in $ millions}
#' \item{vdesc}{variable description}
#' }
#'
#' @details
#' The data for 1994+ are from:
#'
#'   The Census Bureau's API, at https://api.census.gov/data/timeseries/eits/qtax. (For data available via api see: https://api.census.gov/data.html.) The API is not always available (see https://apimetrics.io/2017/04/07/census-data-api-is-down/).
#'
#' @examples
#'   head(qtax_slgus)
#'   comment(qtax_slgus)
"qtax_slgus"
