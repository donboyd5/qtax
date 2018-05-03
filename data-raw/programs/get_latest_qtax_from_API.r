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
#                Get and organize the latest qtax data from the Census API ####
#****************************************************************************************************
# Do this with every quarterly update
# get qtax - EVERYTHING from 1994 forward (that is when the API starts)
# we want to end up with a nice, clean data frame with:
# stabbr (chr), date (date), ic (chr), and value (dbl)

# For data available via api see: https://api.census.gov/data.html

# Here is a Census Bureau example:
# https://api.census.gov/data/timeseries/eits/qtax?get=cell_value,data_type_code,time_slot_id,error_data,category_code,seasonally_adj&for=us:*&time=2012&key=YOUR_KEY_GOES_HERE

censusapikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"

u1 <- "https://api.census.gov/data/timeseries/eits/qtax"
u2 <- "?get=cell_value,data_type_code,time_slot_id,error_data,geo_level_code,category_code,seasonally_adj"
u3 <- ""       # to just get US:  u3 <- "&for=us:*"
# u4 <- "&time=from+1987+to+2017" # note that qtax from api starts 1994q1 but this starts in 1987 just to be safe
u4 <- "&time=from+1992"
u5 <- paste0("&key=", censusapikey)
url <- paste0(u1, u2, u3, u4, u5)
url

system.time(qtdat <- jsonlite::fromJSON(url)) # returns a matrix

# look at the matrix
dim(qtdat)
qtdat[1:2, ]
qtdat[c(1, nrow(qtdat)), ] # 1992q1 start

# create a cleaned-up data frame
qapi1 <- data.frame(qtdat) %>% as_tibble()
glimpse(qapi1)
names(qapi1) <- t(qapi1[1, ])
qapi1 <- qapi1[-1, ]
glimpse(qapi1)
ht(qapi1)
count(qapi1, geo_level_code)
count(qapi1, geo_level_code, category_code) %>% spread(category_code, n) # we have US in cat3 - good

# clean the data: JUST keep the cat3 data
qapi2 <- qapi1 %>%
  filter(category_code=="QTAXCAT3") %>%
  mutate(value=as.numeric(as.character(cell_value)),
         date=as.Date(paste(str_sub(time, 1, 4), as.numeric(str_sub(time, 7, 7))*3-2, 1, sep="-")),
         stabbr=as.character(geo_level_code),
         ic=as.character(data_type_code)) %>%
  select(stabbr, date, ic, value) %>%
  arrange(stabbr, date, ic)
glimpse(qapi2)
summary(qapi2)


# Save this file before moving on because the Census API is not always working, and good to have
# historical files to fall back on.
fn <- paste0("qtfromapi_", Sys.Date(), ".rds") # format(Sys.Date(), "%d%b%Y")
saveRDS(qapi2, paste0("./data/", fn))


#****************************************************************************************************
#                Combine the new data with the historical data ####
#****************************************************************************************************
# Do this with every quarterly update

# define api file to use (can use a prior file)
fn <- paste0("qtfromapi_", Sys.Date(), ".rds") # format(Sys.Date(), "%d%b%Y")

qapi <- readRDS(paste0("./data/", fn))
glimpse(qapi)
summary(qapi)

icodes <- readRDS("./data-raw/icodes.rds")
glimpse(icodes)

qth <- readRDS("./data-raw/qtaxhist_useforpackage.rds")
glimpse(qth)

qall <- bind_rows(qth, qapi2)
glimpse(qall)

qall %>%
  filter(stabbr=="CT", ic=="TOTAL") %>%
  ggplot(aes(date, value)) + geom_line()

qtax <- qall %>%
  left_join(icodes) %>%
  select(stabbr, date, ic, vname, value, vdesc)
glimpse(qtax)
ht(qtax)
qtax %>% select(-value) %>% anyDuplicated()


comment(qtax) <- paste0("Quarterly tax data updated from Census API as of: ", Sys.Date())
comment(qtax)
use_data(qtax, overwrite = TRUE)


