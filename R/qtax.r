#' Quarterly tax data.
#'
#' Quarterly tax revenue data for individual state governments and the U.S. in aggregate, by tax type, from 1963 (U.S.) through recent.
#'
#' @format Data frame with 1 row per state, per tax, per quarter.
#' \describe{
#' \item{stabbr}{state abbreviation}
#' \item{date}{first day of quarter}
#' \item{ic}{item code for the variable, as defined by Census Bureau}
#' \item{vname}{variable name (a memorable name for the item code, ic)}
#' \item{value}{amount in $ millions}
#' \item{vdesc}{variable description}
#' }
#'
#' @source
#' Census Bureau API for 1994+ data, plus various Census Bureau historical files for earlier data.
#'
#' The historical data are assembled from various Census Bureau files - mostly the spreadsheets that are or were
#' on their website at https://www.census.gov/programs-surveys/qtax.html. Some of the historical data go back to 1963.
#'
#' The data for 1994+ are from the Census Bureau's API, at https://api.census.gov/data/timeseries/eits/qtax.
#'
#' One issue with the API data, and with the related "Indicators" data at
#' https://www.census.gov/econ/currentdata/dbsearch?program=QTAX&startYear=1992&endYear=2018&categories=QTAXCAT1&dataType=TOTAL&geoLevel=US&adjusted=0&notAdjusted=1&errorData=0 (also see
#' https://www.census.gov/econ/currentdata/datasets/QTAX-mf.zip at https://www.census.gov/econ/currentdata/datasets/index), is that the Census Bureau dropped 3 significant digits from the data, relative to the spreadsheet data that are at https://www.census.gov/programs-surveys/qtax.html (e.g., the spreadsheet number of $1,234,567 thousand may be recorded in the API data as $1,235 million).
#'
#' Thus, if you are looking at a small tax in a small state, some year-over-year changes may be quite misleading because of the relatively few significant digits. I have raised this with the Census Bureau many times, but it is not a high enough priority for them to fix it. I use the API data for recent years despite this problem because (a) the precision problem usually is not important for most analyses, and (b) the API data are much easier to work with than the spreadsheet data, which are posted in a manner that is not computer-friendly.
#'
#' Another issue with the Census Bureau data is that the quality is often quite poor, as my colleague, Lucy Dadayan, has documented extensively and repeatedly. The Census Bureau does not appear to have the staff resources needed to improve the data. Eventually, I will integrate the data in this package with Lucy Dadayan's quarterly tax data for recent years, which are much more reliable than the Census Bureau's data, but have fewer tax elements (only the major taxes).
#'
#' \strong{At present, the qtax file does not include U.S. totals for 1992 and 1993. I expect to calculate them as the sum of state values, in a later iteration of this package.}
#'
#'
#' @examples
#'   head(qtax)
"qtax"
