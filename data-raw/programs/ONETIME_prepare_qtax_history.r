# Create a package with state-level quarterly tax data
# 5/3/2018

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
# library("precis")

library("tibbletime") # https://business-science.github.io/tibbletime/

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)
library("bdata")


#****************************************************************************************************
#                ONETIME: Create streamlined historical dataset, from data I assembled long ago ####
#****************************************************************************************************
# Long ago, I assembled historical qtax data from spreadsheet files the Census Bureau put on their
# web page. It has state qtax data plus national state-local. The state records include both DC and US.
# However, DC is NOT in the reported sums

# We will use state data from 1963-01-01 through xxxx.
# We will drop the state-local records.
# We will drop the DC records.
# We will drop the source column.

# The original set of spreadsheet files is (or was) in D:/Data/CensusFinanceData/QTax/
# and the historical R files I created were in D:/Data/CensusFinanceData/QTax/qtax rfiles/

# I have copied the key result of that effort, qtaxhistory_best.rds, to the "./data-raw" directory
# of this project. It is the starting point for the streamlined historical data.

qthist <- readRDS(file=paste0("./data-raw/", "qtaxhistory_best.rds"))
glimpse(qthist)
summary(qthist)
# data run from 1963-01-01 to 1993-10-01

# investigate what we want to drop or change
count(qthist, level) # both sg and slg -- we will drop slg
count(qthist, stabbr) # we have DC and also US
count(qthist, stabbr, level) %>% spread(level, n) # for US there are both sg and slg data; for states, we have only sg
count(qthist, source)

# do US sums match sum of states, within a small tolerance?
mismatch <- qthist %>%
  filter(stabbr!="DC", level=="sg") %>%
  mutate(group=ifelse(stabbr=="US", "US", "sumstates")) %>%
  group_by(date, ic, group) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  spread(group, value) %>%
  mutate(ums=US - sumstates,
         pdiff=ums / sumstates * 100) %>%
  filter(abs(pdiff) > 1)

mismatch
# US TOTAL is larger than sum of states in certain cases:
# To be explored further as time allows.
# date       ic    sumstates    US   ums pdiff
# <date>     <chr>     <dbl> <dbl> <dbl> <dbl>
#   1 1979-04-01 TOTAL    34637. 35831 1194.  3.45
# 2 1980-04-01 TOTAL    36536. 39385 2849.  7.80
# 3 1986-07-01 TOTAL    51407. 54834 3427.  6.67
# 4 1986-10-01 TOTAL    55487. 58055 2568.  4.63
# 5 1987-01-01 TOTAL    57804. 62234 4430.  7.66
# 6 1987-04-01 TOTAL    70117. 72395 2278.  3.25
# 7 1987-07-01 TOTAL    57366. 59048 1682.  2.93
# 8 1987-10-01 TOTAL    57280. 62232 4952.  8.64
# 9 1988-01-01 TOTAL    62796. 65947 3151.  5.02
# 10 1988-07-01 TOTAL    61766. 62681  915.  1.48
# 11 1988-10-01 TOTAL    64574. 65812 1238.  1.92
# 12 1990-01-01 TOTAL    74576. 76212 1636.  2.19
# 13 1990-04-01 TOTAL    82703. 86378 3675.  4.44
# 14 1990-07-01 TOTAL    70287. 71474 1187.  1.69
# 15 1990-10-01 TOTAL    69732. 73057 3325.  4.77
# 16 1991-01-01 TOTAL    76253. 77115  862.  1.13
# 17 1991-04-01 TOTAL    85708. 88491 2783.  3.25
# 18 1991-10-01 TOTAL    74735. 77978 3243.  4.34

# could these oddball cases include DC in the US total???
mm2 <- qthist %>%
  filter(date %in% mismatch$date, level=="sg") %>%
  mutate(group=ifelse(stabbr=="US", "US", "sumstates")) %>%
  group_by(date, ic, group) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  spread(group, value) %>%
  mutate(ums=US - sumstates,
         pdiff=ums / sumstates * 100)

# Weird. Now we are much closer except for a few quarters.


# MY SOLUTION: replace the U.S. totals on the file with calculated us totals, in all cases without DC.

qth2 <- qthist %>%
  filter(level=="sg", stabbr %in% state.abb) %>%
  select(stabbr, date, ic, value)

usrecs <- qth2 %>%
  group_by(date, ic) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  mutate(stabbr="US")

qth3 <- bind_rows(qth2, usrecs) %>%
  filter(!is.na(value)) %>%
  arrange(stabbr, date, ic)

summary(qth3)
count(qth3, stabbr)
count(qth3, ic)
count(qth3, date) %>% data.frame
# why such weird counts?
qth3 %>%
  filter(date %in% c("1992-04-01", "1992-07-01", "1992-10-01")) %>%
  group_by(stabbr, ic) %>%
  summarise(n=n())

qth3 %>%
  filter(date %in% as.Date(c("1992-04-01", "1992-07-01", "1992-10-01"))) %>%
  group_by(stabbr, ic, date) %>%
  summarise(n=n()) %>%
  spread(date, n) %>%
  ht(30)

qth3 %>% filter(date=="1992-07-01", stabbr=="AK")

saveRDS(qth3, "./data-raw/qtaxhist_useforpackage.rds")

