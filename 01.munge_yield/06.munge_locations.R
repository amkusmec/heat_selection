library(tidyverse)
library(readxl)


d <- read_csv("data/trial_data/munged_trials_all.csv")

# Distinct locations
ref <- d %>%
  distinct(State, Region, County, Town)

# Identify regions, counties, and towns that are present in `df` but not `ref`
miss <- function(df, ref) {
  list(Regions = sort(setdiff(df$Region, ref$Region)), 
       Counties = sort(setdiff(df$County, ref$County)), 
       Towns = sort(setdiff(df$Town, ref$Town)))
}

# Filter `df`
filter_locations <- function(df, miss) {
  df <- df %>%
    filter(!is.na(Region), !is.na(County), !is.na(Town))
  
  miss <- lapply(miss, function(x) x[!is.na(x)])
  
  if (length(miss$Region) > 0) {
    df <- df %>%
      filter(!(Region %in% miss$Region))
  }
  if (length(miss$County) > 0) {
    df <- df %>%
      filter(!(County %in% miss$County))
  }
  if (length(miss$Town) > 0) {
    df <- df %>%
      filter(!(Town %in% miss$Town))
  }
  
  df
}


# Illinois ----------------------------------------------------------------
IL <- read_xlsx("data/trial_data/metadata/IL_Locations_and_dates_SoilTypes_LA_020818.xlsx", 
                col_types = c("numeric", "text", "text", "text", "date", "date", "text", "text", "skip", "skip"), 
                na = c("", "NOT AVAILABLE", "NA")) %>%
  mutate_at(c("Region", "County", "Town"), str_to_upper) %>%
  mutate(Town = str_replace(Town, "BROWNSTON", "BROWNSTOWN") %>%  
           str_replace(., "ELIXABETHTOWN", "ELIZABETHTOWN") %>%
           str_replace(., "GRAYS LAKE", "GRAYSLAKE"), 
         County = str_replace(County, "ADAMS", "HANCOCK") %>%
           str_replace(., "MACOUPIN", "GREENE") %>%
           str_replace(., "JOHNSON", "POPE"))

IL_miss <- miss(IL, filter(ref, State == "ILLINOIS"))
IL <- filter_locations(IL, IL_miss) %>%
  mutate(State = "ILLINOIS") %>%
  select(State, Year:Harvested)


# Kansas ------------------------------------------------------------------
districts <- tibble(District = paste("District", 1:7), 
                    Region = c("Northeast", "East Central", "Southeast", 
                               "North Central", "South Central", "Northwest", 
                               "Southwest")) %>% 
  mutate_all(str_to_upper)

KS <- read_xlsx("data/trial_data/metadata/KS_Locations_and_dates_SoilTypes_LA_061317.xlsx", 
                col_types = c("numeric", "text", "text", "text", "date", "date", "text", "text", "skip", "skip"), 
                na = c("", "NA")) %>%
  rename(Region = District) %>%
  mutate_at(c("Region", "County", "Town"), str_to_upper) %>%
  mutate(Region = str_replace(Region, "-", " ") %>%
           str_remove(., "ERN") %>% 
           str_remove(., " KANSAS"), 
         Region = if_else(str_detect(Region, "DISTRICT"), 
                          districts$Region[match(Region, districts$District, nomatch = NA)], 
                          Region),
         County = str_remove(County, "\\([A-Z]*[:blank:]?[A-Z]*\\)") %>% str_trim(),
         County = if_else(County == "CROWFORD", "CRAWFORD", County), 
         County = if_else(County == "GRRELEY", "GREELEY", County), 
         County = if_else(Town == "PARSONS", "LABETTE", County), 
         County = if_else(Town == "MANKATO", "JEWELL", County), 
         County = if_else(Town == "TOPEKA", "SHAWNEE", County), 
         County = if_else(Town == "GARNETT", "ANDERSON", County), 
         County = if_else(Town == "SCANDIA", "REPUBLIC", County), 
         County = if_else(Town == "EMMETT", "POTTAWATOMIE", County), 
         County = if_else(Town == "BELLEVILLE", "REPUBLIC", County),
         County = if_else(str_detect(County, "STAFFORD"), "STAFFORD", County), 
         Town = str_remove(Town, " \\(IRRIGATED\\)"))

KS_miss <- miss(KS, filter(ref, State == "KANSAS"))
KS <- filter_locations(KS, KS_miss) %>%
  mutate(State = "KANSAS") %>%
  select(State, Year:Harvested)


# Nebraska ----------------------------------------------------------------
NE_ref <- filter(d, State == "NEBRASKA") %>%
  distinct(Year, Region, County, Town)
NE <- read_xlsx("data/trial_data/metadata/NE_Locations_dates_SoilTypes_Irrigation_03072019.xlsx", 
                col_types = c("numeric", "text", "text", "text", "date", "date", "text", "text", "skip", "skip"), 
                na = c("", "N/A", "-")) %>%
  rename(Year = YEAR, Region = REGION, County = COUNTY, Town = TOWN) %>%
  mutate_at(c("Region", "County", "Town"), str_to_upper) %>%
  mutate(Town = str_replace(Town, "ITHICA", "ITHACA") %>% 
           str_replace(., "ODESA", "ODESSA") %>% 
           str_replace(., "RANDOLP", "RANDOLPH") %>% 
           str_replace(., "SHELTON", "SHELDON") %>%
           str_replace(., "TALMADGE", "TALMAGE") %>% 
           str_replace(., "WEST TORRINGTON", "TORRINGTON") %>% 
           str_replace(., "DOGE", "DODGE") %>% 
           str_replace(., "THRUSTON", "THURSTON") %>% 
           str_replace(., "PLATTESMOUTH", "PLATTSMOUTH") %>% 
           str_replace(., "CENTEA", "CENTER") %>% 
           str_replace(., "BRAINNARD", "BRAINARD") %>% 
           str_replace(., "DEWITT", "DE WITT")) %>%
  mutate(County = if_else(Town == "UNION", "CASS", County), 
         County = if_else(Town == "PALISADE", "HITCHCOCK", County), 
         County = if_else(Town == "VALPARAISO", "SAUNDERS", County), 
         County = if_else(Town == "BEE", "SEWARD", County), 
         County = if_else(Town == "EDGAR", "CLAY", County), 
         County = if_else(Town == "BENKELMAN", "DUNDY", County), 
         County = if_else(Town == "WAUNETA", "CHASE", County), 
         County = if_else(Town == "CENTER", "KNOX", County), 
         County = if_else(Town == "BELDEN", "CEDAR", County), 
         County = if_else(Town == "MADISON", "MADISON", County), 
         County = if_else(Town == "RANDOLPH", "CEDAR", County), 
         County = if_else(Town == "HOLBROOK", "FURNAS", County), 
         County = if_else(Town == "COOK", "JOHNSON", County), 
         County = if_else(Town == "CONCORD", "DIXON", County), 
         County = if_else(Town == "NORTH PLATTE", "LINCOLN", County), 
         County = if_else(Town == "NICKERSON", "DODGE", County), 
         County = if_else(Town == "DE WITT", "SALINE", County), 
         County = if_else(Town == "COLUMBIA", "KNOX", County))

NE_miss <- miss(NE, filter(ref, State == "NEBRASKA"))
NE_table <- filter(NE, Region %in% NE_miss$Regions) %>%
  select(Year:Town) %>%
  distinct(Year, Region, County, Town) %>%
  inner_join(., NE_ref, by = c("Year", "County", "Town"))
NE <- left_join(NE, NE_table, 
                by = c("Year", "Region" = "Region.x", "County", "Town")) %>%
  mutate(Region = if_else(is.na(Region.y), Region, Region.y))

NE_miss <- miss(NE, filter(ref, State == "NEBRASKA"))
NE <- filter_locations(NE, NE_miss) %>%
  mutate(State = "NEBRASKA") %>%
  select(State, Year:Harvested)


# Iowa --------------------------------------------------------------------
IA <- read_xlsx("data/trial_data/metadata/iowa_locations.xlsx", 
                col_types = c("numeric", "text", "text", "text", "date", "date", 
                              "numeric", "numeric"), na = c("", "NA")) %>%
  rename(Region = District) %>%
  mutate(across(c("Region", "County", "Town"), str_to_upper)) %>% 
  mutate(Town = str_replace(Town, "ROLE", "ROLFE") %>%
           str_replace(., "CRESECENT", "CRESCENT") %>% 
           str_replace(., "LEMARS", "LE MARS") %>% 
           str_replace(., "SERGEANT BLUFFS", "SERGEANT BLUFF") %>% 
           str_replace(., "LAPORTE CITY", "LA PORTE CITY"))

IA1 <- filter(IA, Year < 1955 | Year > 2004) %>%
  mutate(across(c("Planting Date", "Harvest Date", 
                  "Latitude", "Longitude"), as.character)) %>%
  mutate(County = str_replace(County, "SIOUX", "O'BRIEN"), 
         County = if_else(Town == "WEST BRANCH", "CEDAR", County), 
         County = if_else(Town == "RUSSELL", "LUCAS", County), 
         County = if_else(Town == "MABEL", "FILLMORE", County), 
         County = if_else(Town == "MAPLETON", "MONONA", County), 
         County = if_else(Town == "NEW VIRGINIA", "WARREN", County))

count_nas <- function(x) {
  if_else(str_count(x, "NA") == (str_count(x, ";") + 1), 
          as.character(NA), x)
}

county_swap <- function(x, y, tt = "WEST BRANCH", cc = "CEDAR") {
  idx <- str_which(y, tt)
  if (length(idx) == 0) x
  
  x[idx] <- cc
  x
}

IA2 <- filter(IA, Year >= 1955, Year <= 2004) %>%
  group_by(Year, Region) %>%
  summarise(across(County:Longitude, ~ paste(.x, collapse = "; ")), 
            .groups = "drop") %>%
  mutate(across(Town:Longitude, count_nas)) %>% 
  mutate(across(c(Town, County), ~ str_split(.x, "; "))) %>%
  mutate(County = map2(County, Town, county_swap, tt = "WEST BRANCH", cc = "CEDAR"), 
         County = map2(County, Town, county_swap, tt = "RUSSELL", cc = "LUCAS"), 
         County = map2(County, Town, county_swap, tt = "SHELDON", cc = "O'BRIEN"), 
         County = map2(County, Town, county_swap, tt = "MABEL", cc = "FILLMORE"), 
         County = map2(County, Town, county_swap, tt = "NEW VIRGINIA", cc = "WARREN"), 
         County = map2(County, Town, county_swap, tt = "MAPLETON", cc = "MONONA"), 
         County = map2(County, Town, county_swap, tt = "ESTHERVILLE", cc = "EMMET"), 
         County = map2(County, Town, county_swap, tt = "WEST LIBERTY", cc = "MUSCATINE"), 
         County = map2(County, Town, county_swap, tt = "CEDAR", cc = "MAHASKA")) %>%
  mutate(across(c(Town, County), ~ map_chr(.x, paste, collapse = "; ")))

IA <- bind_rows(IA1, IA2)

IA_miss <- miss(IA, filter(ref, State == "IOWA"))
IA <- filter_locations(IA, IA_miss) %>%
  mutate(State = "IOWA") %>%
  select(State, Year:Longitude) %>%
  rename(Planted = `Planting Date`, Harvested = `Harvest Date`)


# Assemble the cleaned location table -------------------------------------
locations <- bind_rows(IL, KS, NE) %>%
  mutate(Latitude = as.character(NA), Longitude = as.character(NA)) %>%
  mutate(across(c("Planted", "Harvested"), as.character)) %>%
  bind_rows(., IA)

write_csv(locations, "data/trial_data/metadata/munged_locations.csv")
