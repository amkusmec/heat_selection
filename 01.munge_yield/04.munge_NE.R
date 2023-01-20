library(tidyverse)
library(readxl)


# Load the data sheets
file <- "data/trial_data/Nebraska/NE-CornPerformanceData_2016-1948_CURATED.xlsx"
sheets <- excel_sheets(file)
d <- sheets %>%
  map(function(s) {
    temp <- read_xlsx(file, sheet = s, na = c("-", "*", "NA", "", "---", "N/A"))
    temp$`YIELD (BU/A)` <- as.numeric(temp$`YIELD (BU/A)`)
    temp
  })
names(d) <- sheets

# Identify common column names
cc <- lapply(d, names) %>%
  unlist() %>% 
  enframe() %>% 
  count(value) %>%
  filter(n == 66) %>%
  pull(value) %>%
  sort()

# Possible markers of irrigation
irrigation <- c("IRRIGATED", "FURROW", "PIVOT", "FURROW IRRIGATED", "PIVOT IRRIGATED", 
                "FURROW IRRIGATION", "PIVOT IRRIGATION", "GRAVITY IRRIGATION", 
                "IRRIGATED LONG SEASON", "IRRIGATED SHORT SEASON", "LRRIGATED", 
                "IRRIGATED; EARLY HYBRID", "IRRIGATED: EARLY HYBRIDS")

# Combine everything and filter
d <- map_df(d, function(df) df[, cc]) %>%
  filter(!is.na(`YIELD (BU/A)`), !is.na(COUNTY), !is.na(STATE), 
         !is.na(YEAR), !str_detect(VARIETY, "FARM ENTRY"), !is.na(VARIETY), 
         !str_detect(COUNTY, "BT/RW"), !is.na(BRAND), !is.na(REGION), 
         !str_detect(BRAND, "CHECK"), !str_detect(BRAND, "FARM ENTRY"), 
         !str_detect(BRAND, "POPULAR ENTRY"), !str_detect(BRAND, "^YELLOW"), 
         !str_detect(BRAND, "^YC")) %>%
  filter(!(IRRIGATION %in% irrigation)) %>%
  mutate_at(c("BRAND", "COUNTY", "IRRIGATION", "REGION", "STATE", "TOWN", "VARIETY"), 
            str_to_upper) %>%
  mutate(BRAND = if_else(BRAND == "VINEYARD", "VINEYARD SEED CO.", BRAND), 
         SEASON = NA) %>% 
  select(BRAND:COUNTY, REGION, SEASON, STATE:`YIELD (BU/A)`)
names(d) <- c("Brand", "County", "Region", "Season", "State", 
              "Town", "Variety", "Year", "Yield (bu/a)")

# Fix town and county names
d <- d %>%
  mutate(Town = str_replace(Town, "DOGE", "DODGE") %>% 
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
         County = if_else(Town == "DE WITT", "SALINE", County))

write_csv(d, "data/trial_data/munged_NE.csv")
