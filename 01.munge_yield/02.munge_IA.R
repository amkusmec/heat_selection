library(tidyverse)
library(readxl)

# Perform rudimentary processing and renaming of data sheets
files <- list.files("data/trial_data/Iowa", "*", full.names = TRUE)
d <- files %>%
  map(function(f) {
    sheets <- excel_sheets(f)
    map(sheets[str_detect(sheets, "^[0-9]{4}$") & !str_detect(sheets, "^192[0-9]$")], function(s) { 
      temp <- read_xlsx(f, sheet = s, na = c("-", "*", "NA", "N/A", "---", ""))
      names(temp) <- str_replace(names(temp), "Brand...4", "Brand")
      names(temp) <- str_replace(names(temp), "Brand...6", "Brand")
      names(temp) <- str_replace(names(temp), "Variety...5", "Variety")
      names(temp) <- str_replace(names(temp), "Variety...7", "Variety")
      names(temp) <- str_replace(names(temp), "Yield \\(Bu/A\\)", "Yield (bu/a)")
      names(temp) <- str_replace(names(temp), "Percent Yield", "Yield (bu/a)")
      names(temp) <- str_replace(names(temp), "Region...3", "Region")
      temp$`Yield (bu/a)` <- as.numeric(temp$`Yield (bu/a)`)
      temp
    })
  })
names(d) <- str_remove(files, "data/trial_data/Iowa/")

# Identify columns common to all datasheets
cc <- lapply(d, function(l) { 
      lapply(l, names) 
    }) %>%
  unlist() %>% 
  enframe() %>% 
  count(value) %>%
  filter(n == 85) %>%
  pull(value)
cc <- sort(c(cc, "Town", "County", "Location"))

# Combine all the data sheets and filter missing values
d <- d %>%
  map_df(function(x1) {
      map_df(x1, function(x2) {
          if (!("Town" %in% names(x2))) x2$Town <- NA
          if (!("County" %in% names(x2))) x2$County <- NA
          if (!("Location" %in% names(x2))) x2$Location <- NA
          x2[, cc]
        })
    }) %>% 
  filter(Year >= 1932, !is.na(Variety), !is.na(Brand), 
         !is.na(`Yield (bu/a)`), !str_detect(Brand, "PENDING")) %>%
  mutate(Region = if_else(str_detect(Location, "D[Ii][Ss]trict"), Location, Region), 
         County = if_else(!str_detect(Location, "D[Ii][Ss]trict"), Location, County)) %>%
  select(-TestWt, -Location) %>%
  mutate_at(c("Brand", "County", "Region", "Season", "State", "Town", "Variety"), str_to_upper) %>%
  mutate(Brand = if_else(Brand == "MELLOWDENT", "MELLOW DENT", Brand), 
         Brand = if_else(Brand == "STURDY GROW", "STURDY-GROW", Brand), 
         Brand = if_else(Brand == "VIKING", "VIKING SEED", Brand)) %>%
  select(-Irrigation)

# Add location data
ia_locationsA <- read_xlsx("data/trial_data/metadata/iowa_locations.xlsx", 
                           col_types = c("numeric", "text", "text", "text", 
                                         "date", "date", "numeric", "numeric"), 
                           na = c("", "NA")) %>%
  filter(Year < 1955 | Year > 2004) %>%
  select(Year:Town) %>%
  mutate_at(c("District", "County", "Town"), str_to_upper)
ia_locationsB <- read_xlsx("data/trial_data/metadata/iowa_locations.xlsx", 
                           col_types = c("numeric", "text", "text", "text", 
                                         "date", "date", "numeric", "numeric"), 
                           na = c("", "NA")) %>%
  filter(Year >= 1955, Year <= 2004) %>%
  group_by(Year, District) %>%
  summarise(County = paste(County, collapse = "; "), 
            Town = paste(Town, collapse = "; "), 
            .groups = "drop") %>%
  ungroup() %>%
  mutate(Town = if_else(str_detect(Town, "NA"), as.character(NA), Town)) %>%
  mutate_at(c("District", "County", "Town"), str_to_upper)

dA <- filter(d, Year < 1955 | Year > 2004) %>%
  inner_join(., ia_locationsA, by = c("Year", "Town")) %>%
  select(Brand, County.y, District, Season:`Yield (bu/a)`) %>%
  rename(County = County.y, Region = District)
dB <- filter(d, Year >= 1955, Year <= 2004) %>%
  select(-County, -Town) %>%
  left_join(., ia_locationsB, by = c("Year", "Region" = "District")) %>%
  filter(!is.na(County), !is.na(Town)) %>%
  select(Brand, County, Region:State, Town, Variety:`Yield (bu/a)`)

# Correct town names
d <- bind_rows(dA, dB) %>%
  mutate(Town = str_replace(Town, "ROLE", "ROLFE") %>%
           str_replace(., "CRESECENT", "CRESCENT") %>% 
           str_replace(., "LEMARS", "LE MARS") %>% 
           str_replace(., "SERGEANT BLUFFS", "SERGEANT BLUFF") %>% 
           str_replace(., "LAPORTE CITY", "LA PORTE CITY"))

# Correct county names
county_swap <- function(x, y, tt = "WEST BRANCH", cc = "CEDAR") {
  idx <- str_which(y, tt)
  if (length(idx) == 0) x
  
  x[idx] <- cc
  x
}

d <- d %>%
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

write_csv(d, "data/trial_data/munged_IA.csv")
