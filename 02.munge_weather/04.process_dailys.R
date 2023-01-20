library(tidyverse)
library(lubridate)
library(parallel)
options(readr.show_progress = FALSE)

### Reformat and filter the GHCND daily files.
###  - Keep only March-September (3-9) (DISREGARD)
###  - Keep the entire year's data for calculation of future planting dates
###  - Count the number of missing values per month

# files <- list.files("data/ghcnd/dailys", "*\\.csv", full.names = TRUE)
# missing <-  mclapply(files, function(f) {
#       options(readr.show_progress = FALSE)
#       cat(which(files == f), "/", length(files), "\r")
# 
#       # Subset by variable and month and remove the source flag
#       d <- read_csv(f, progress = FALSE,
#                     col_types = paste0("ciic", paste(rep("nccc", times = 31), collapse = ""))) %>%
#         filter(ELEMENT %in% c("TMAX", "TMIN", "PRCP")) %>%
#         # filter(MONTH >= 3, MONTH <= 9) %>%
#         select(-contains("SFLAG"))
# 
#       if (nrow(d) == 0) {
#         return(tibble(ID = NA, YEAR = NA, MONTH = NA,
#                       TMAX_MISSING = NA, TMIN_MISSING = NA,
#                       PRCP_MISSING = NA))
#       }
# 
#       # Separate out the value and flag columns for reformatting
#       value <- d %>%
#         select(ID:ELEMENT, contains("VALUE")) %>%
#         pivot_longer(contains("VALUE"), names_to = "DAY", values_to = "VALUE") %>%
#         mutate(DAY = str_remove(DAY, "VALUE") %>% as.integer,
#                VALUE = if_else(VALUE == -9999, as.numeric(NA), VALUE))
# 
#       mflags <- d %>%
#         select(ID:ELEMENT, contains("MFLAG")) %>%
#         pivot_longer(contains("MFLAG"), names_to = "DAY", values_to = "MFLAG") %>%
#         mutate(DAY = str_remove(DAY, "MFLAG") %>% as.integer)
# 
#       qflags <- d %>%
#         select(ID:ELEMENT, contains("QFLAG")) %>%
#         pivot_longer(contains("QFLAG"), names_to = "DAY", values_to = "QFLAG") %>%
#         mutate(DAY = str_remove(DAY, "QFLAG") %>% as.integer)
# 
#       # Recombine the data table and convert units of the variables
#       #  - TMAX/TMIN are in tenths of a degree Celsius
#       #  - PRCP is in tenths of a mm
#       # This step generates date parsing warnings. These are due to the inclusion
#       # of DAY = 31 for April, June, and September which don't have 31 days. This
#       # creates an invalid date that fails to parse.
#       d <- reduce(list(value, mflags, qflags), inner_join,
#                   by = c("ID", "YEAR", "MONTH", "DAY", "ELEMENT")) %>%
#         mutate(VALUE = if_else(MFLAG %in% c("A", "L"), as.numeric(NA), VALUE),
#                VALUE = if_else(!is.na(QFLAG), as.numeric(NA), VALUE),
#                VALUE = VALUE/10) %>%
#         select(-contains("FLAG")) %>%
#         pivot_wider(names_from = "ELEMENT", values_from = "VALUE",
#                     values_fill = list("VALUE" = as.numeric(NA))) %>%
#         mutate(YDAY = paste(YEAR, MONTH, DAY, sep = "-") %>%
#                  ymd() %>% yday()) %>%
#         filter(!is.na(YDAY)) %>%
#         select(-YDAY)
# 
#       # For consistency, check to see if all three variables are present
#       m <- setdiff(c("TMAX", "TMIN", "PRCP"), names(d))
#       if (length(m) > 0) {
#         for (x in m) {
#           d <- d %>%
#             mutate(!! x := as.numeric(NA))
#         }
# 
#         d <- d %>%
#           select(ID:DAY, TMAX, TMIN, PRCP)
#       }
# 
#       # Save the reformatted data
#       write_csv(d, paste0("data/ghcnd/reformatted/", d$ID[1], "_reformat.csv"))
# 
#       # Count missing values by year, month, and variable
#       d %>%
#       # read_csv(f) %>%
#         group_by(ID, YEAR, MONTH) %>%
#         summarise(TMAX_MISSING = sum(is.na(TMAX)),
#                   TMIN_MISSING = sum(is.na(TMIN)),
#                   PRCP_MISSING = sum(is.na(PRCP)),
#                   .groups = "drop")
# 
#     }, mc.cores = detectCores()/2) %>%
#   bind_rows()
# write_csv(missing, "data/ghcnd/stations_missing_step1.csv")

# n_months <- 9 # January to September inclusive
# missing <- read_csv("data/ghcnd/stations_missing_step1.csv")
# 
# idx <- missing %>%
#   pull(ID) %>%
#   unique() %>%
#   rep_along(1:(detectCores()/2)) %>%
#   sort()
# missing <- tibble(IDX = idx,
#                   ID = missing %>% pull(ID) %>% unique) %>%
#   inner_join(missing, by = "ID") %>%
#   split(.$IDX)
# 
# res <- mclapply(missing, function(df) {
#       df %>%
#         filter(MONTH <= 9) %>% 
#         group_by(ID, YEAR) %>%
#         summarise(
#           tibble(N_MONTHS = n(),
#                  across(contains("MISSING"), ~ sum(.x <= 3))),
#           .groups = "drop")
#     }, mc.cores = detectCores()/2) %>%
#   bind_rows() %>%
#   filter(N_MONTHS == n_months)
# write_csv(res, "data/ghcnd/stations_missing_step2.csv")


res <- read_csv("data/ghcnd/stations_missing_step2.csv")
tmax <- res %>%
  filter(TMAX_MISSING == 9) %>%
  group_by(YEAR) %>%
  summarise(STATIONS = list(ID),
            .groups = "drop") %>%
  filter(YEAR >= 1930)

tmin <- res %>%
  filter(TMIN_MISSING == 9) %>%
  group_by(YEAR) %>%
  summarise(STATIONS = list(ID),
            .groups = "drop") %>%
  filter(YEAR >= 1930)

prcp <- res %>%
  filter(PRCP_MISSING == 9) %>%
  group_by(YEAR) %>%
  summarise(STATIONS = list(ID),
            .groups = "drop") %>%
  filter(YEAR >= 1930)

write_rds(list(TMAX = tmax, TMIN = tmin, PRCP = prcp),
          "data/ghcnd/stations_missing_final.rds")
