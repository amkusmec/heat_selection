library(tidyverse)
library(lubridate)
library(raster)
library(parallel)

options(readr.show_progress = FALSE)

### Link PRISM grid cells to the counties where trials occur
# Trial locations and dates
trials <- read_csv("data/trial_data/metadata/locations_gps.csv", 
                   col_types = "ciccccccc") %>%
  mutate(across(c(Planted:Harvested, Latitude:Longitude), ~ str_split(.x, "; "))) %>%
  unnest(c(Planted:Harvested, Latitude:Longitude)) %>%
  mutate(across(Planted:Harvested, as_date)) %>%
  mutate(across(Latitude:Longitude, as.numeric)) %>%
  mutate(PMONTH = month(Planted), 
         HMONTH = if_else(month(Harvested) > 9, 9, month(Harvested))) %>%
  filter(PMONTH < HMONTH) %>%
  filter(!is.na(Latitude), !is.na(Longitude))

# Link trials to county IDs
counties <- shapefile("data/prism/county_outlines/tl_2017_us_county.shp")
county_intersect <- trials %>%
  dplyr::select(Longitude, Latitude) %>%
  SpatialPoints(proj4string = CRS(projection(counties))) %>%
  sp::over(counties)

# Identify unique trials on a state-year-county basis
# Restrict harvest dates to the end of September
trials_unique <- trials %>%
  mutate(COUNTYNS = county_intersect$COUNTYNS) %>%
  group_by(State, Year, COUNTYNS) %>%
  summarise(County = list(County), 
            Planted = min(Planted), 
            Harvested = min(Harvested, ymd(paste0(Year, "-09-30"))), 
            Latitude = list(Latitude), 
            Longitude = list(Longitude), 
            .groups = "drop")
write_rds(trials_unique, "data/trial_data/trials_unique.rds")

# Pick fixed planting/harvesting dates for comparison
p_fixed <- trials_unique %>%
  filter(month(Planted) == 4) %>%
  mutate(Day = day(Planted)) %>%
  count(Day) %>%
  mutate(Prop = cumsum(n)/sum(n)) %>% 
  filter(Prop >= 0.5) %>%
  slice(1L) %>%
  pull(Day)
h_fixed <- "9-30"

# Get PRISM-county mappings
prism_files <- list.files("data/prism/distributions", "*\\.csv") %>%
  str_split("_") %>%
  map_chr(`[`, 2) %>%
  as.integer()
prism <- list.files("data/prism/subset", "*\\.csv", full.names = TRUE) %>%
  `[`(1) %>% 
  read_csv() %>%
  filter(Cell %in% prism_files) %>%
  distinct(COUNTYNS, Cell) %>%
  group_by(COUNTYNS) %>%
  summarise(Cells = list(Cell)) %>%
  ungroup()

rm(counties, county_intersect, trials, prism_files)
gc()


res <- mclapply(1:nrow(trials_unique), function(i) {
      cat("County", i, "\n")
      
      # Pull the cells for the county
      cells <- prism %>%
        filter(COUNTYNS == trials_unique$COUNTYNS[i]) %>%
        pull(Cells) %>%
        unlist()
      
      # Date ranges
      p_date <- trials_unique$Planted[i]
      h_date <- trials_unique$Harvested[i]
      p_fixed_date <- paste(trials_unique$Year[i], "4", p_fixed, sep = "-") %>% ymd()
      h_fixed_date <- paste(trials_unique$Year[i], h_fixed, sep = "-") %>% ymd()
      
      # Cell-level distributions
      df <- map_df(cells, function(j) {
          paste0("data/prism/distributions/PRISM_", j, "_distribution.csv") %>%
            read_csv(col_types = cols()) %>% 
            mutate(DATE = paste(YEAR, MONTH, DAY, sep = "-") %>% ymd(), 
                   Cell = j) %>%
            filter(YEAR == trials_unique$Year[i])
        })
      
      df_var <- df %>%
        filter(DATE >= p_date, DATE <= h_date) %>% 
        group_by(Cell) %>%
        summarise(across(c(PRCP, starts_with("X")), ~ sum(.x, na.rm = TRUE))) %>%
        summarise(across(-Cell, ~ mean(.x, na.rm = TRUE))) %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
        mutate(Season = "Variable")
      
      df_fix <- df %>%
        filter(DATE >= p_fixed_date, DATE <= h_fixed_date) %>% 
        group_by(Cell) %>%
        summarise(across(c(PRCP, starts_with("X")), ~ sum(.x, na.rm = TRUE))) %>%
        summarise(across(-Cell, ~ mean(.x, na.rm = TRUE))) %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
        mutate(Season = "Fixed")
      
      # County average distribution
      bind_rows(df_var, df_fix) %>%
        mutate(Year = trials_unique$Year[i], 
               State = trials_unique$State[i], 
               County = trials_unique$COUNTYNS[i]) %>%
        dplyr::select(Season, Year:County, Variable:Value)
    }, mc.cores = detectCores()) %>%
  bind_rows()


write_csv(res, "data/prism/all_trials_distributions.csv")
