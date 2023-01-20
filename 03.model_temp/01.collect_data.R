library(tidyverse)
library(lubridate)
library(raster)


# Censor the weather data -------------------------------------------------
weather <- read_csv("data/prism/all_trials_distributions.csv")

# Separate out the precipitation
prcp_lower <- 250
prcp_upper <- 800

prcp <- filter(weather, Variable == "PRCP") %>%
  arrange(Year, State, County) %>%
  dplyr::select(-Variable) %>% 
  mutate(Value = if_else(Value < prcp_lower, prcp_lower, Value), 
         Value = if_else(Value > prcp_upper, prcp_upper, Value)) %>% 
  pivot_wider(names_from = "Season", values_from = "Value") %>% 
  filter(if_all(Fixed:Variable, ~ !is.na(.x)))

# Left- and right-censor the temperature distributions
temp_lower <- 0
temp_upper <- 40

weather <- weather %>% 
  filter(Variable != "PRCP") %>%
  mutate(Variable = str_remove(Variable, "X") %>%
           str_replace("\\.", "-") %>%
           as.integer()) %>%
  arrange(Year, State, County, Variable) %>%
  rename(Degree = Variable, Time = Value) %>%
  group_by(Year, State, County, Season) %>%
  group_modify(~ {
      temp_low <- filter(.x, Degree < temp_lower) %>%
        pull(Time) %>%
        sum(na.rm = TRUE)
      temp_high <- filter(.x, Degree > temp_upper) %>%
        pull(Time) %>%
        sum(na.rm = TRUE)
      filter(.x, Degree >= temp_lower, 
             Degree <= temp_upper) %>%
        bind_rows(., 
                  tibble(Degree = c(temp_lower - 1, temp_upper + 1), 
                         Time = c(temp_low, temp_high))) %>%
        arrange(Degree)
    }) %>%
  ungroup() %>% 
  mutate(Time = 24*Time) %>%  # Convert days to hours
  pivot_wider(names_from = "Season", values_from = "Time") %>%
  filter(if_all(Fixed:Variable, ~ !is.na(.x)))

site_bins <- weather %>% 
  count(Year, State, County) %>% 
  filter(n == (length(temp_lower:temp_upper) + 2))

weather <- inner_join(weather, 
                      dplyr::select(site_bins, -n), 
                      by = c("Year", "State", "County"))

# weather %>% 
#   pivot_longer(Fixed:Variable, names_to = "Season", values_to = "Time") %>% 
#   ggplot(aes(x = Degree, y = Time)) + theme_bw() +
#     geom_boxplot(aes(group = Degree), outlier.shape = 1, outlier.colour = "red") +
#     scale_x_continuous(breaks = seq(-30, 50, 5)) + 
#     facet_wrap(~ Season, ncol = 1, strip.position = "right")

# # Do the same plot with the differences
# weather %>% 
#   mutate(Difference = Fixed - Variable) %>% 
#   ggplot(aes(x = Degree, y = Difference)) + theme_bw() + 
#     geom_boxplot(aes(group = Degree), outlier.shape = 1, outlier.colour = "red") + 
#     scale_x_continuous(breaks = seq(-30, 50, 5))


# Identify trial county centroids -----------------------------------------
trials_unique <- read_rds("data/trial_data/trials_unique.rds")
counties <- shapefile("data/prism/county_outlines/tl_2017_us_county.shp")

centroids <- rgeos::gCentroid(counties, byid = TRUE)
centroids <- as_tibble(centroids@coords) %>%
  rename(Lon_cent = x, Lat_cent = y) %>%
  mutate(COUNTYNS = counties@data$COUNTYNS) %>%
  filter(COUNTYNS %in% trials_unique$COUNTYNS)

trials_unique <- inner_join(trials_unique, centroids, by = "COUNTYNS")


# Filter and align the data tables ----------------------------------------
yield <- read_csv("data/trial_data/munged_trials_all.csv") %>% 
  filter(Year >= 1934, Year < 2015) %>% 
  filter(`Yield (bu/a)` > 0) %>% 
  inner_join(trials_unique %>% 
               dplyr::select(State:County) %>% 
               unnest(County), 
             by = c("Year", "State", "County"))

# Location keys
keys_prcp <- prcp %>% 
  unite(Key, Year:County, sep = "_", remove = FALSE) %>% 
  pull(Key) %>% 
  unique()
keys_weather <- weather %>% 
  unite(Key, Year:County, sep = "_", remove = FALSE) %>% 
  pull(Key) %>% 
  unique()
keys_trials <- trials_unique %>% 
  unite(Key, c(Year, State, COUNTYNS), sep = "_", remove = FALSE) %>% 
  pull(Key) %>% 
  unique()
keys_yield <- yield %>% 
  unite(Key, c(Year, State, COUNTYNS), sep = "_", remove = FALSE) %>% 
  pull(Key) %>% 
  unique()

keys <- objects(pattern = "keys") %>% 
  lapply(sym) %>% 
  lapply(eval) %>% 
  reduce(intersect)

# Filter
prcp <- prcp %>% 
  unite(Key, Year:County, sep = "_", remove = FALSE) %>% 
  filter(Key %in% keys)
weather <- weather %>% 
  unite(Key, Year:County, sep = "_", remove = FALSE) %>% 
  filter(Key %in% keys)
trials_unique <- trials_unique %>% 
  unite(Key, c(Year, State, COUNTYNS), sep = "_", remove = FALSE) %>% 
  filter(Key %in% keys)
yield <- yield %>% 
  unite(Key, c(Year, State, COUNTYNS), sep = "_", remove = FALSE) %>% 
  filter(Key %in% keys)


# Rename and reorder the filtered tables ----------------------------------
prcp <- prcp %>%
  dplyr::select(-Key)
weather <- weather %>% 
  dplyr::select(-Key) %>% 
  rename(Temperature = Degree)
trials_unique <- trials_unique %>%
  dplyr::select(Year, State, COUNTYNS, Lon_cent:Lat_cent, Planted:Harvested) %>%
  rename(County = COUNTYNS, Longitude = Lon_cent, Latitude = Lat_cent)
yield <- yield %>%
  dplyr::select(Year, State, COUNTYNS, Variety, `Yield (bu/a)`) %>%
  rename(County = COUNTYNS, Yield = `Yield (bu/a)`)


# Apply QC filters --------------------------------------------------------
# Time-series and cross-section filters: each observational unit must occur at 
# least twice for identifiability
county_filt <- trials_unique %>% 
  group_by(County) %>% 
  summarise(Year = length(unique(Year)), 
            .groups = "drop") %>% 
  filter(Year >= 2) %>% 
  pull(County)
year_filt <- trials_unique %>% 
  group_by(Year) %>% 
  summarise(County = length(unique(County)), 
            .groups = "drop") %>% 
  filter(County >= 2) %>% 
  pull(Year)

yield2 <- yield %>% 
  filter(County %in% county_filt, Year %in% year_filt)

# Variety filters: jointly observed in at least two locations and two years
variety_filt1 <- yield2 %>% 
  group_by(Variety) %>% 
  summarise(across(c(Year, County), ~ length(unique(.x))), 
            .groups = "drop") %>% 
  filter(if_all(Year:County, ~ .x >= 2)) %>% 
  pull(Variety)

yield3 <- yield2 %>% 
  filter(Variety %in% variety_filt1)

# Variety filters: at least 15 observations
variety_filt2 <- yield3 %>% 
  count(Variety) %>% 
  filter(n >= 15) %>% 
  pull(Variety)

yield4 <- yield3 %>% 
  filter(Variety %in% variety_filt2)

# Final checks
yield4 %>% group_by(Year) %>% summarise(County = length(unique(County))) %>% pull(County) %>% all(. >= 2)
yield4 %>% group_by(Year) %>% summarise(Variety = length(unique(Variety))) %>% pull(Variety) %>% all(. >= 2)

yield4 %>% group_by(County) %>% summarise(Year = length(unique(Year))) %>% pull(Year) %>% all(. >= 2)
yield4 %>% group_by(County) %>% summarise(Variety = length(unique(Variety))) %>% pull(Variety) %>% all(. >= 2)


# Filter other data tables ------------------------------------------------
keys2 <- yield4 %>% 
  mutate(Key = paste(Year, State, County, sep = "_")) %>% 
  pull(Key) %>% 
  unique()

prcp2 <- prcp %>% 
  unite(Key, Year:County, sep = "_", remove = FALSE) %>% 
  filter(Key %in% keys2) %>% 
  dplyr::select(-Key)
weather2 <- weather %>% 
  unite(Key, Year:County, sep = "_", remove = FALSE) %>% 
  filter(Key %in% keys2) %>% 
  dplyr::select(-Key)
trials_unique2 <- trials_unique %>% 
  unite(Key, Year:County, sep = "_", remove = FALSE) %>% 
  filter(Key %in% keys2) %>% 
  dplyr::select(-Key)


# Write the final tables --------------------------------------------------
write_csv(prcp2, "data/strict/precipitation_noI.csv")
write_csv(weather2, "data/strict/temperature_noI.csv")
write_csv(yield4, "data/strict/yield_noI.csv")
write_csv(trials_unique2, "data/strict/trials_noI.csv")


# Create the combined table -----------------------------------------------
yield4 <- yield4 %>% 
  mutate(LogYield = log(Yield))

prcp2 <- prcp2 %>% 
  dplyr::select(-Fixed) %>% 
  rename(PRCP = Variable)

temperature <- weather2 %>% 
  dplyr::select(-Fixed) %>% 
  rename(Value = Variable) %>% 
  group_by(Year, State, County) %>% 
  summarise(TotalHours = sum(Value), 
            Ly = list(Value), 
            Lt = list(Temperature), 
            .groups = "drop")

trials_unique2 <- trials_unique2 %>% 
  mutate(PDAY = lubridate::yday(Planted))

join_vars <- c("Year", "State", "County")
reg_dat <- reduce(list(yield4, prcp2, temperature, trials_unique2), 
                  inner_join, by = join_vars)
write_rds(reg_dat, "data/strict/regression_data.rds")
