library(tidyverse)
options(readr.show_progress = FALSE)

### Retrieve HCN daily files for all stations in all states with trials and all
### states surrounding those trials. Restrict to stations that never moved by
### more than 0.035 degrees of latitude or longitude.

# Two-letter state abbreviations
states <- c("CO", "IA", "IL", "IN", "KS", "KY", "MI", "MN", "MO", "NE", "OK", 
            "SD", "WI", "WY")


# Create an inventory of possible stations --------------------------------
### Station list
stations <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt" %>%
  read_fwf(fwf_cols(ID = c(1, 11), LATITUDE = c(13, 20), LONGITUDE = c(22, 30), 
                    ELEVATION = c(32, 37), STATE = c(39, 40), NAME = c(42, 71), 
                    GSN_FLAG = c(73, 75), HCN_CRN_FLAG = c(77, 79), 
                    WMO_ID = c(81, 85)), 
           col_types = "cdddccccc") %>%
  filter(str_detect(ID, "^US")) %>%
  filter(STATE %in% states)

# Identify stations that may have moved
station_counts <- stations %>%
  mutate(KEY = paste(STATE, NAME, sep = "_")) %>%
  count(KEY) %>%
  filter(n > 1)

moved <- stations %>%
  mutate(KEY = paste(STATE, NAME, sep = "_")) %>%
  filter(KEY %in% station_counts$KEY) %>%
  group_by(KEY) %>%
  summarise(LAT_DIFF = diff(LATITUDE) %>% abs() %>% max(), 
            LONG_DIFF = diff(LONGITUDE) %>% abs() %>% max()) %>%
  ungroup() %>%
  filter(LAT_DIFF > 0.035 | LONG_DIFF > 0.035)

# Removed stations that moved too far
stations <- stations %>%
  mutate(KEY = paste(STATE, NAME, sep = "_")) %>%
  filter(!KEY %in% moved$KEY) %>%
  select(-KEY)

### Get the station inventory for high level filtering of stations
inventory <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt" %>%
  read_fwf(fwf_cols(ID = c(1, 11), LATITUDE = c(13, 20), LONGITUDE = c(22, 30), 
                    ELEMENT = c(32, 35), FIRSTYEAR = c(37, 40), LASTYEAR = c(42, 45)), 
           col_types = "cddcii") %>%
  filter(ID %in% stations$ID) %>%
  filter(ELEMENT %in% c("TMAX", "TMIN", "PRCP"))

### Save the lists
write_csv(stations, "data/ghcnd/station_list.csv")
write_csv(inventory, "data/ghcnd/station_inventory.csv")


# Download daily records for the candidate stations -----------------------
prefix <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/"
stem <- ".dly"

# Fixed-width column names and positions
NAMES = c("ID", "YEAR", "MONTH", "ELEMENT", 
          paste0(rep(c("VALUE", "MFLAG", "QFLAG", "SFLAG"), times = 31), 
                 rep(1:31, each = 4))) 
START <- c(1, 12, 16, 18, rep(NA, times = 124))
END <- c(11, 15, 17, 21, rep(NA, times = 124))

for (i in 5:length(START)) {
  START[i] <- END[i - 1] + 1
  END[i] <- START[i] + 
    switch(str_remove(NAMES[i], "[0-9]{1,2}$"), 
           VALUE = 4, 
           MFLAG = 0, 
           QFLAG = 0, 
           SFLAG = 0)
}

daily_cols <- map2(.x = START, .y = END, function(x, y) c(x, y))
names(daily_cols) <- NAMES

# Fixed-width column types
daily_types <- paste0("ciic", paste(rep("iccc", times = 31), collapse = ""))

# Download the data
n_cores <- 40
split_idx <- split(stations$ID, sort(rep_along(stations$ID, 1:n_cores))) %>%
  map2(.x = ., .y = 1:n_cores, function(x, y) append(x, y, after = 0))

res <- parallel::mclapply(split_idx, function(idx) {
  for (i in 2:length(idx)) {
    cat("Process", idx[1], ":", i, "/", length(idx), "\n")
    
    res <- NULL
    tryCatch({
      res <- R.utils::withTimeout({
        paste0(prefix, idx[i], stem) %>%
          read_fwf(rlang::exec(fwf_cols, !!! daily_cols), col_types = daily_types) %>%
          write_csv(., paste0("data/ghcnd/dailys/", idx[i], ".csv"))
      }, timeout = 30, onTimeout = "error")
    }, TimeoutException = function(ex) message("Timeout. Skipping."), 
    error = function(e) message("Error. Skipping."))
    
    # Wait to avoid overloading the FTP server
    Sys.sleep(2)
  }
}, mc.cores = n_cores)
