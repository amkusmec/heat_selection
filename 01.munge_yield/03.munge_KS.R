library(tidyverse)
library(readxl)

# Table for converting district names
districts <- tibble(District = paste("District", 1:7), 
                    Region = c("Northeast", "East Central", "Southeast", 
                               "North Central", "South Central", "Northwest", 
                               "Southwest"))

# Remove two years with bad data
files <- list.files("data/trial_data/Kansas", "*", full.names = TRUE) %>%
  setdiff(., "data/trial_data/Kansas/11_KansasCornTrials_1971-1972.xlsx")

# Load files and do lots of column renaming
d <- files %>%
  map(function(f) { 
    sheets <- excel_sheets(f) 
    map(sheets[str_detect(sheets, "^[0-9]{4}$")], function(s) {
      temp <- read_xlsx(f, sheet = s, na = c("-", "*", "NA", "", "N/A", "---"))
      
      names(temp) <- str_replace(names(temp), "Location", "County")
      names(temp) <- str_replace(names(temp), "Vareity", "Variety")
      names(temp) <- str_replace(names(temp), "Season...9", "Season")
      names(temp) <- str_replace(names(temp), "Season...10", "Season")
      names(temp) <- str_replace(names(temp), "Irrigation...9", "Irrigation")
      names(temp) <- str_replace(names(temp), "Irrigation...14", "Irrigation")
      names(temp) <- str_replace(names(temp), "BRAND", "Brand")
      names(temp) <- str_replace(names(temp), "Region...3", "Region")
      names(temp) <- str_replace(names(temp), "Disctrict", "District")
      names(temp) <- str_replace(names(temp), "ActualYield \\(bu\\)", "Yield (bu/a)")
      names(temp) <- str_replace(names(temp), "Yield\\(Bu\\)", "Yield (bu/a)")
      names(temp) <- str_replace(names(temp), "Actual Yield \\(bu\\)", "Yield (bu/a)")
      names(temp) <- str_replace(names(temp), "YIELD\\(\\(bu/a\\)\\)", "Yield (bu/a)")
      
      if(any(str_detect(names(temp), "[Aa]cre"))) {
        names(temp)[str_detect(names(temp), "[Aa]cre")] <- "Yield (bu/a)"
      }
      
      if("District" %in% names(temp) & !("Region" %in% names(temp))) {
        temp <- left_join(temp, districts, by = "District")
      }
      
      temp$`Yield (bu/a)` <- as.numeric(temp$`Yield (bu/a)`)
      temp
    })
  })
names(d) <- str_remove(files, "data/trial_data/Kansas/")

# Identify common column names across all data sheets
cc <- lapply(d, function(l) { 
      lapply(l, names) 
    }) %>%
  unlist() %>% 
  enframe() %>% 
  count(value) %>%
  filter(n >= 74) %>%
  pull(value) %>%
  sort()

# Combine all sheets and do some filtering
d <- d %>%
  map_df(function(x1) {
      map_df(x1, function(x2) x2[, cc])
    }) %>%
  filter(!is.na(Brand), !is.na(Variety), !is.na(Year), !is.na(`Yield (bu/a)`),
         !is.na(Town), !str_detect(Brand, "CHECK"), !str_detect(Brand, "^LOCAL"), 
         !str_detect(Brand, "CHK")) %>%
  filter(is.na(Irrigation) | Irrigation == "Dryland") %>%
  select(-Irrigation) %>%
  mutate(County = if_else(is.na(County), "Sedgwick", County), 
         State = "Kansas") %>%
  mutate_at(c("Brand", "Variety", "Town", "County", "State", "Region", "Season"), 
            str_to_upper) %>%
  mutate(Brand = if_else(Brand == "MIDLAND" & Year <= 1952, "MIDLAND OPV", Brand), 
         Brand = if_else(Brand == "U. S." | Brand == "U.S.", "US", Brand), 
         Region = if_else(Region == "EAST-CENTRAL" | Region == "EAST/CENTRAL", 
                          "EAST CENTRAL", Region), 
         Region = if_else(Region == "NORTHWESTERN", "NORTHWEST", Region), 
         Region = if_else(Region == "SOUTH EAST" | Region == "SOUTHEASTERN", 
                          "SOUTHEAST", Region), 
         Region = if_else(Region == "SOUTH WEST", "SOUTHWEST", Region))

# Fix county names
d <- d %>%
  mutate(County = if_else(Town == "PARSONS", "LABETTE", County), 
         County = if_else(Town == "MANKATO", "JEWELL", County), 
         County = if_else(Town == "TOPEKA", "SHAWNEE", County), 
         County = if_else(Town == "GARNETT", "ANDERSON", County), 
         County = if_else(Town == "SCANDIA", "REPUBLIC", County), 
         County = if_else(Town == "EMMETT", "POTTAWATOMIE", County), 
         County = if_else(Town == "BELLEVILLE", "REPUBLIC", County))

write_csv(d, "data/trial_data/munged_KS.csv")
