#' Quarterly State Government Tax Revenue Data.
#'
#' Quarterly tax revenue data for individual state governments and the U.S. in aggregate, by tax type, from 1963 for selected items, through most recent.
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
#' @details
#' I assembled the historical data from various Census Bureau files - mostly the spreadsheets that are now or once were on their website at https://www.census.gov/programs-surveys/qtax.html. Some of the historical data go back to 1963. Most or possibly all of these files, and associated documentation, can be found at https://www2.census.gov/govs/qtax/.
#' The data for 1994+ are from one of two sources:
#'
#'   1) The Census Bureau's API, at https://api.census.gov/data/timeseries/eits/qtax. (For data available via api see: https://api.census.gov/data.html.) The API is not always available (see https://apimetrics.io/2017/04/07/census-data-api-is-down/).
#'
#'   2) A zip file that the Census Bureau maintains, with essentially the same information, for its "Economic Indicators" series: see https://www.census.gov/econ/currentdata/datasets/QTAX-mf.zip at https://www.census.gov/econ/currentdata/datasets/index.
#'
#' There is a third possible source of recent data that I do not use: spreadsheet data that are at https://www.census.gov/programs-surveys/qtax.html. I do not use the spreadsheet data because they currently are posted in a manner that is not computer friendly - the spreadhsheets have to be parsed one quarter at a time, and there are enough changes in format over time that it is a potentially error-prone activity.
#'
#' One issue with the API and zip data is that the Census Bureau has dropped 3 significant digits from the data, relative to the spreadsheet data. For example, the spreadsheet number of $1,234,567 thousand may be recorded in the API and zip data as $1,235 million. Thus, if you are looking at a small tax in a small state, some year-over-year changes may be quite misleading because of the relatively few significant digits. I have raised this with the Census Bureau many times, but it is not a high enough priority for them to fix it.
#'
#' I use the API and/or zip data for recent years despite this problem because (a) the precision problem usually is not important for most analyses, and (b) the API/zip data are much easier to work with than the spreadsheet data.
#'
#' Another issue with the Census Bureau data is that the quality is often quite poor, as my colleague, Lucy Dadayan, has documented extensively and repeatedly. The Census Bureau does not appear to have the staff resources needed to improve the data. Eventually, I will integrate the data in this package with Lucy Dadayan's quarterly tax data for recent years, which are much more reliable than the Census Bureau's data, but have fewer tax elements (only the major taxes).
#'
#' There appear to be some oddities in some quarters. For example, in some quarters, the Census Bureau has records for the Alaska sales tax that are zero, consistent with Alaska's lack of a general sales tax. In a few quarters, the tax is near-zero but not precisely zero. I err on the side of caution and leave these kinds of records in the data.
#'
#' @examples
#'   head(qtax)
#'   comment(qtax)
"qtax"
