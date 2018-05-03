
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
#                Get the already-created recent data that was obtained from the API or from a zip file ####
#****************************************************************************************************
#.. If from the API:
# fn <- paste0("qtfromapi_", Sys.Date(), ".rds") # format(Sys.Date(), "%d%b%Y")

#.. If from the zip file:
fn <- "qtax_fromzip.rds"


# read the new file:
qnew <- readRDS(paste0("./data-raw/", fn))

count(qnew, stabbr)
count(qnew, ic)


#****************************************************************************************************
#                Combine the new data with the historical data ####
#****************************************************************************************************
# Do this with every quarterly update

glimpse(qnew)
summary(qnew)

icodes <- readRDS("./data-raw/icodes.rds")
glimpse(icodes)

qth <- readRDS("./data-raw/qtaxhist_useforpackage.rds")
glimpse(qth)

qall <- bind_rows(qth, qnew)
glimpse(qall)

qall %>%
  filter(stabbr=="US", ic=="TOTAL") %>%
  ggplot(aes(date, value)) + geom_line()

qtax <- qall %>%
  left_join(icodes) %>%
  select(stabbr, date, ic, vname, value, vdesc)
glimpse(qtax)
ht(qtax)
qtax %>% select(-value) %>% anyDuplicated()

qtax %>% count(date) %>% data.frame

comment(qtax) <- paste0("Quarterly tax data updated from Census API as of: ", Sys.Date())
comment(qtax)
use_data(qtax, overwrite = TRUE)

