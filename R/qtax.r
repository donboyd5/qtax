#' qtax Quarterly tax data.
#'
#' qtax Quarterly tax revenue data for state governments and the U.S. total, by tax type, from 1963 (U.S.) through recent.
#'
#' @source Census Bureau API for recent , plus various historial files.
#'
#' @format Data frame with 1 row per variable per date, NA records have been removed.
#' \describe{
#' \item{stabbr}{chr: state abbreviation}
#' \item{date}{date: first day of quarter}
#' \item{ic}{chr: item code for the variable, as defined by Census Bureau}
#' \item{vname}{chr: variable name}
#' \item{value}{double: $ amount, in units described in FOF documentation}
#' \item{vdesc}{chr: variable description}
#' }
#' @examples
#'   head(qtax)
"qtax"
