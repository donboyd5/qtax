# Create a package with state-level quarterly tax data
# 5/2/2018

# stabbr date source ic value level

# Note that historical state totals appear to be available only from around 1977 forward,
# but U.S. totals are available from the early 1960s.


# Package authoring with RStudio:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("devtools")
# devtools::check(cleanup = FALSE) # when we want to see check output no matter what


library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times.
library("lubridate") # lubridate, for date/times.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
library("precis")

library("tibbletime") # https://business-science.github.io/tibbletime/

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)
library("bdata")


#****************************************************************************************************
#                ONE-TIME: Get qtax item codes, short names, and descriptions, and save ####
#****************************************************************************************************
icodes <- read_csv(
"ic, vname, vdesc
T01, proptax, Property taxes
T09, gst, General sales and gross receipts
T10, alcbevtax, Alcoholic beverages tax
T11, amusementstax, Amusements
T12, ipt, Insurance premiums
T13, mft, Motor fuels
T14, pmt, Pari-mutuels
T15, utiltax, Public utilities tax
T16, cigtax, Tobacco products
T19, otherselsalestax, Other selective sales and gross receipts
T20, alcbevlic, Alcoholic beverages licenses
T21, amusementslic, Amusements
T22, corpfee, Corporations in general
T23, huntfishfee, Hunting and fishing
T24, mvfee, Motor vehicles licenses
T24T25, mvfeeoplic, Motor vehicles fees & operators' licenses
T25, mvoplic, Motor vehicle operators licenses
T27, utillic, Public utilities licenses
T28, occbustax, Occupation and businesses
T29, othrlic, Other license taxes
T40, iit, Individual income
T41, cit, Corporation net income
T50, egt, Death and gift
T51, stt, Documentary and stock transfer
T53, sevtax, Severance
T99, othrtaxnec, Other taxes NEC
TOTAL, tottax, Total taxes")
icodes

saveRDS(icodes, "./data-raw/icodes.rds")



