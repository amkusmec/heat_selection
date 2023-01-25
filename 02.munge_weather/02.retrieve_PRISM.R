library(tidyverse)
library(lubridate)
library(prism)


# Find the date ranges to retrieve data for -------------------------------
trials <- read_csv("data/trial_data/metadata/locations_gps.csv", 
                   col_types = "ciccccccc")

# Years
min_year <- min(trials$Year)
max_year <- max(trials$Year)

min_month <- 1

max_month <- trials$Harvested %>%
  str_split("; ") %>%
  unlist() %>%
  as_date() %>%
  month() %>%
  max(na.rm = TRUE)

# Actual maximum month is 12 (December) due to some really late harvests. Setting 
# this to 9 (September) to reflect the typical growing season.
max_month <- 9


# Retrieve files from the PRISM database ----------------------------------
prism_set_dl_dir("data/prism/raw", create = FALSE)
get_prism_monthlys("tmin", years = min_year:max_year, mon = min_month:max_month, 
                   keepZip = FALSE)
get_prism_monthlys("tmax", years = min_year:max_year, mon = min_month:max_month, 
                   keepZip = FALSE)
get_prism_monthlys("ppt", years = min_year:max_year, mon = min_month:max_month, 
                   keepZip = FALSE)
