library(tidyverse)
library(readxl)


# Remove years where only 2- and 3-year averages are reported
files <- list.files("data/trial_data/Illinois", "*", full.names = TRUE) %>%
  setdiff(., "data/trial_data/Illinois/13.IL_CornPerformance_1962-1969_DONE.xlsx")

# Read files and perform some rudimentary processing
d <- files %>%
  map(function(f) {
    sheets <- excel_sheets(f)
    map(sheets, function(s) {
      temp <- read_xlsx(f, sheet = s, na = c("-", "*", "NA", "N/A", "---", "", "M"))
      names(temp) <- str_replace(names(temp), "Year...1", "Year")
      names(temp) <- str_replace(names(temp), "Brand...6", "Brand")
      temp
    })
  })
names(d) <- str_remove(files, "data/trial_data/Illinois/")

# Identify columns common to all years
cc <- lapply(d, function(l) { 
      lapply(l, names) 
    }) %>%
  unlist() %>% 
  enframe() %>% 
  count(value) %>%
  filter(n == 73) %>%
  pull(value) %>%
  sort()

# Combine all of the data sheets, filter missing values, and remove irrigated trials
d <- d %>%
  map_df(function(x1) {
      map_df(x1, function(x2) x2[, cc])
    }) %>%
  filter(!is.na(Brand), !is.na(County), !is.na(Variety), !is.na(`Yield (bu/a)`), 
         !is.na(Region)) %>%
  mutate(State = "Illinois") %>%
  mutate_at(c("County", "Region", "State", "Town"), str_to_upper) %>%
  filter(Region != "NOT AVAILABLE", is.na(Irrigation)) %>%
  select(-Irrigation)

# Fix errors in location names
d <- d %>% 
  mutate(Town = str_replace(Town, "GRAYS LAKE", "GRAYSLAKE")) %>% 
  mutate(County = if_else(Town == "BOWEN", "HANCOCK", County), 
         County = if_else(Town == "GREENFIELD", "GREENE", County), 
         County = if_else(Town == "DIXON SPRINGS", "POPE", County))

# Save the results
write_csv(d, "data/trial_data/munged_IL.csv")
