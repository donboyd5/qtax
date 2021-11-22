
# code folding ----
# alt-o, shift-alt-o
# alt-l, shift-alt-l
# alt-r


# notes ----
# we want state gov only (category 3), exclude DC
# 1994+
# US = sum of states
# state, date, ic, value
# ic are 3 characters or TOTAL


#****************************************************************************************************
# Libraries ####
#****************************************************************************************************
library(devtools)
# devtools::check(cleanup = FALSE) # when we want to see check output no matter what


library(magrittr)
library(plyr) # needed for ldply; must be loaded BEFORE dplyr
library(tidyverse)
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library(scales)
library(hms) # hms, for times.
library(lubridate) # lubridate, for date/times.
library(readxl) # readxl, for .xls and .xlsx files.
library(haven) # haven, for SPSS, SAS and Stata files.
library(vctrs)
library(precis)

library(tibbletime) # https://business-science.github.io/tibbletime/

library(grDevices)
library(knitr)

library(zoo) # for rollapply

library(btools) # library that I created (install from github)
library(bdata)


#****************************************************************************************************
# Get zip data ####
#****************************************************************************************************
fnz <- "QTAX-mf.zip"
fnc <- "QTAX-mf.csv"

ubase <- "https://www.census.gov/econ/currentdata/datasets/"

url <- paste0(ubase, fnz)

pathz <- paste0("./data-raw/", fnz)

download.file(url, pathz, mode="wb")


#****************************************************************************************************
# Read the zip data ####
#****************************************************************************************************
# use readlines to read all into text
# we want to have stabbr, date, ic, value

txt <- readLines(unz(pathz, fnc))
length(txt)
txt[1]
txt[2]
txt[3]

# find the segments in txt
segs <- read_delim("segname; seg
                   cats; cat_idx,cat_code,cat_desc,cat_indent
                   dtype; dt_idx,dt_code,dt_desc,dt_unit
                   geo; geo_idx,geo_code,geo_desc
                   periods; per_idx,per_name
                   data; per_idx,cat_idx,dt_idx,geo_idx,is_adj,val",
                   delim=";", trim_ws=TRUE)
segs

# now add the start and end location
g <- function(seg) {
  starts <- which(str_count(txt, seg)>0)
  if(length(starts)>1)  stop("Not unique!!!")
  df <- tibble(seq=seg, start=starts[1])
  return(df)
}

segs2 <- segs %>%
  group_by(segname) %>%
  do(g(.$seg)) %>%
  arrange(start) %>%
  ungroup %>%
  mutate(end=lead(start) - 1,
         end=ifelse(segname=="data", length(txt), end))
segs2

# now get each file segment
getseg <- function(segname) {
  start <- segs2$start[segs2$segname==segname]
  end <- segs2$end[segs2$segname==segname]
  s <- txt[start : end]
  s <- paste(s, collapse="\n")
  df <- read_csv(s)
  return(df)
}

cats <- getseg("cats") %>%
  mutate(cat_idx=as.integer(cat_idx)) %>%
  filter(!is.na(cat_idx))
cats

dtype <- getseg("dtype") %>%
  mutate(dt_idx=as.integer(dt_idx)) %>%
  filter(!is.na(dt_idx))
dtype

geo <- getseg("geo") %>%
  mutate(geo_idx=as.integer(geo_idx)) %>%
  filter(!is.na(geo_idx))
geo

periods <- getseg("periods") %>%
  mutate(per_idx=as.integer(per_idx)) %>%
  filter(!is.na(per_idx))
periods %>% ht

data <- getseg("data") %>%
  mutate(per_idx=as.integer(per_idx)) %>%
  filter(!is.na(per_idx)) %>%
  rename(value=val) %>%
  mutate(value=as.numeric(value))
data
glimpse(data)


#****************************************************************************************************
#                Subset, organize, and save the data ####
#****************************************************************************************************
df <- data %>%
  left_join(cats, by = "cat_idx") %>%
  left_join(dtype, by = "dt_idx") %>%
  left_join(geo, by = "geo_idx") %>%
  left_join(periods, by = "per_idx")
glimpse(df)
count(df, is_adj)

df2 <- df %>%
  filter(cat_code=="QTAXCAT3",
         geo_code %in% state.abb,
         (nchar(dt_code)==3) | (dt_code=="TOTAL")) %>%
  select(-ends_with("idx"),
         -is_adj,
         -cat_indent,
         -cat_code,
         -cat_desc,
         -dt_unit, -geo_desc) %>%
  rename(stabbr=geo_code) %>%
  mutate(date=paste(str_sub(per_name, 3, 6),
                    (str_sub(per_name, 2, 2)  %>% as.integer) * 3 -2,
                    1, sep="-") %>%
           as.Date) %>%
  select(-per_name)

glimpse(df2)
summary(df2)
count(df2, date)
count(df2, stabbr) # 50 states, but NOT DC or US
count(df2, dt_code, dt_desc)
ht(df2)

df3 <- df2 %>%
  select(stabbr, date, ic=dt_code, value) %>%
  filter(!is.na(value))

usrecs <- df3 %>%
  group_by(date, ic) %>%
  summarise(value=sum(value, na.rm=TRUE), .groups="drop") %>%
  mutate(stabbr="US")

df4 <- bind_rows(df3, usrecs) %>%
  arrange(stabbr, date, ic, value)
glimpse(df4)
summary(df4) # 1994q1 - 2019q4, fdoq

saveRDS(df4, "./data-raw/qtax_fromzip.rds")





