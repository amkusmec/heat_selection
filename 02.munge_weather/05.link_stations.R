library(tidyverse)
library(lubridate)
library(parallel)
source("src/haversine.R")

### Link PRISM grid cells with the seven (7) closest weather stations having
### continuous records. A continuous record is defined as a station having at
### most one (1) month of the period January-September with more than three (3) 
### missing values. Each variable (TMAX, TMIN, PRCP) can have a different set
### of seven stations.

# Inventory of stations and missing data points
stations_missing <- read_rds("data/ghcnd/stations_missing_final.rds")
stations_list <- read_csv("data/ghcnd/station_list.csv", 
                          col_types = "cdddccccc")

# List of PRISM grid cells
prism_grids <- read_csv(paste0("data/prism/subset/PRISM_ppt_stable_4kmM2_193203", 
                               "_bil_subset.csv")) %>%
  select(COUNTYNS:Latitude)

# Trial locations and dates
trials <- read_csv("data/trial_data/metadata/locations_gps.csv", 
                   col_types = "ciccccccc")
min_year <- min(trials$Year)
max_year <- max(trials$Year)

stations_missing <- stations_missing %>%
  map(function(df) filter(df, YEAR >= min_year, YEAR <= max_year))


# For a given trial:
#  1) Extract PRISM grid cells by matching COUNTYNS
#  2) For each grid cell:
#     a) Compute distance to all stations and sort
#     b) For each variable:
#         i) Iteratively query stations until 7 good stations are accumulated
#        ii) A good station has at most one month with >3 missing values
idx <- prism_grids %>%
  pull(Cell) %>% 
  unique() %>%
  rep_along(1:(detectCores()/2)) %>%
  sort()
prism_grids <- tibble(IDX = idx, 
                      Cell = prism_grids %>% pull(Cell) %>% unique) %>%
  inner_join(prism_grids, by = "Cell") %>%
  split(.$IDX)

# Function to identify the seven closest valid stations for a given weather 
# variable and year
station_match <- function(v, yr, lat, lon) {
  valid_stations <- stations_missing[[v]] %>%
    filter(YEAR == yr) %>%
    pull(STATIONS) %>%
    `[[`(1)
  
  stations_list %>%
    filter(ID %in% valid_stations) %>%
    mutate(DIST = haversine(lon, LONGITUDE, lat, LATITUDE)) %>%
    arrange(DIST) %>%
    slice(1:7) %>%
    pull(ID)
}

station_map <- mclapply(prism_grids, function(df) {
      df %>%
        rowwise() %>%
        mutate(STATIONS = list(tibble(YEAR = min_year:max_year, 
                                      LAT = Latitude, 
                                      LON = Longitude) %>%
                                 rowwise() %>%
                                 mutate(TMAX = list(station_match("TMAX", YEAR, LAT, LON)), 
                                        TMIN = list(station_match("TMIN", YEAR, LAT, LON)), 
                                        PRCP = list(station_match("PRCP", YEAR, LAT, LON))) %>%
                                 ungroup() %>%
                                 select(-LAT, -LON))) %>%
        ungroup()
    }, mc.cores = detectCores()/2) %>%
  bind_rows()

write_rds(station_map, "data/prism/grid_to_station_mapping.rds")
