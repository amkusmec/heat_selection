library(tidyverse)
library(lubridate)
library(VGAM)
library(parallel)
source("src/haversine.R")

options(readr.show_progress = FALSE)


# Identify unique station-variable-year combinations and reformat for 
# efficient imputation
stations_list <- read_csv("data/ghcnd/station_list.csv", 
                          col_types = "cdddccccc")
stations_missing <- read_rds("data/ghcnd/stations_missing_final.rds")

# Transform the list of stations with acceptable levels of missing data into a 
# list of unique stations with the years for which they have valid data for 
# each weather variable
unique_stations <- stations_missing %>% 
  names() %>% 
  map_df(function(v) {
      stations_missing[[v]] %>%
        unnest(STATIONS) %>%
        group_by(STATIONS) %>%
        summarise(YEARS = list(YEAR), 
                  .groups = "drop") %>%
        mutate(VARIABLE = v) %>%
        select(STATIONS, VARIABLE, YEARS)
    }) %>%
  group_by(STATIONS) %>%
  nest()

imputed <- list.files("data/ghcnd/imputed", "*") %>%
  str_remove("_impute.csv")
unique_stations <- unique_stations %>%
  filter(!(STATIONS %in% imputed))

# Catch stations that generate errors
# Impute missing daily weather values
errors <- mclapply(1:nrow(unique_stations), function(i) {
  options(readr.show_progress = FALSE)
  cat("Station", i, "\n")
  idx <- which(stations_list$ID == unique_stations$STATIONS[i])
  
  # Wrap everything in error handling for safety
  tryCatch({
    # Generates parsing failures because daily files contain 31 days for ALL months, 
    # even when a month does not contain 31 days
    d_target <- read_csv(paste0("data/ghcnd/reformatted/", unique_stations$STATIONS[i], 
                                "_reformat.csv"), 
                         col_types = "cnnnnnn") %>%
      mutate(JDAY = paste(YEAR, MONTH, DAY, sep = "-") %>%
               ymd() %>% yday()) %>%
      filter(!is.na(JDAY), 
             MONTH <= 9)
    
    # Variable loop
    for (j in 1:nrow(unique_stations$data[[i]])) {
      v <- unique_stations$data[[i]]$VARIABLE[j]
      years <- unique_stations$data[[i]]$YEARS[[j]]
      
      nas <- which((d_target$YEAR %in% years) & is.na(d_target[[v]]))
      
      # No missing values; no imputation needed
      if (length(nas) == 0) next
      
      # Add half-month fixed effects per Schlenker and Roberts (2009)
      mm <- d_target %>%
        filter(YEAR %in% years) %>%
        mutate(HALFMONTH = MONTH + if_else(DAY <= 15, 0, 0.5)) %>%
        select(YEAR, HALFMONTH, JDAY, !! v)
      
      # Build the model matrix
      # Match the target station with the seven closest stations that have 
      # non-missing values for the days with missing values at the target station
      weather <- unique(mm$YEAR) %>%
        map_df(function(y) {
          y_idx <- which(stations_missing[[v]]$YEAR == y)
          station_dist <- stations_list %>%
            slice(-idx) %>%
            filter(ID %in% stations_missing[[v]]$STATIONS[[y_idx]]) %>%
            mutate(DIST = haversine(stations_list$LONGITUDE[idx], LONGITUDE,
                                    stations_list$LATITUDE[idx], LATITUDE)) %>%
            select(ID, DIST) %>%
            arrange(DIST)
          
          # Missing values in this year
          m_days <- d_target %>%
            filter(YEAR == y) %>%
            filter(is.na(!! sym(v))) %>%
            pull(JDAY)
          
          candidates <- list()
          ptr <- 1
          repeat {
            d_cand <- read_csv(paste0("data/ghcnd/reformatted/", station_dist$ID[ptr], 
                                      "_reformat.csv"), 
                               col_types = "cnnnnnn") %>%
              filter(YEAR == y) %>%
              mutate(HALFMONTH = MONTH + if_else(DAY <= 15, 0, 0.5), 
                     JDAY = paste(YEAR, MONTH, DAY, sep = "-") %>%
                       ymd() %>% yday()) %>%
              select(YEAR, HALFMONTH, JDAY, !! v) %>%
              filter(!is.na(JDAY))
            
            # Are any of the imputed days missing from the predictor station?
            if (!all(!is.na(d_cand[[v]][d_cand$JDAY %in% m_days]))) {
              ptr <- ptr + 1
              next
            }
            
            names(d_cand)[4] <- paste0("X", length(candidates) + 1)
            candidates <- append(candidates, list(d_cand))
            ptr <- ptr + 1
            
            if (length(candidates) == 7) break
          }
          
          reduce(candidates, full_join, by = c("YEAR", "HALFMONTH", "JDAY"))
        }) %>% 
        filter(HALFMONTH < 10)
      
      mm <- mm %>%
        full_join(weather, by = c("YEAR", "HALFMONTH", "JDAY")) %>%
        mutate(HALFMONTH = as.character(HALFMONTH))
      mm_est <- filter(mm, !is.na(!! sym(v)))
      mm_pred <- filter(mm, is.na(!! sym(v)))
      
      # Check for rank deficiencies in the model matrix
      mock_model <- as.formula(paste0(v, " ~ . - YEAR - JDAY")) %>%
        model.matrix(data = mm_est)
      rank_check <- caret::findLinearCombos(mock_model)
      
      if (!is.null(rank_check$remove)) {
        mm_est <- mm_est[, -which(names(mm_est) %in% colnames(mock_model)[rank_check$remove])]
      }
      
      if (v %in% c("TMAX", "TMIN")) {
        # Ordinary least squares for temperature variables
        fit <- lm(as.formula(paste0(v, " ~ . - JDAY - YEAR")), mm_est)
        pred <- predict(fit, mm_pred)
        attr(pred, "names") <- NULL
        d_target[[v]][nas] <- pred
      } else {
        # Tobit regression (left-censored at 0) for precipitation
        fit <- vglm(PRCP ~ . - JDAY - YEAR, tobit(Lower = 0, type.fitted = "censored", 
                                                  imethod = 3, zero = 2), 
                    data = mm_est)
        d_target[[v]][nas] <- drop(predictvglm(fit, mm_pred, type = "response", 
                                               type.fitted = "censored"))
      }
    }
    
    write_csv(d_target, paste0("data/ghcnd/imputed/", unique_stations$STATIONS[i], 
                               "_impute.csv"))
    
    # Everything finished correctly
    TRUE
  }, 
  error = function(e) e)
}, mc.cores = detectCores()/2)

# Report results and exit
n_errors <- sapply(errors, function(x) length(x) != 1) %>% sum()
cat("\nFinished.")
cat(nrow(unique_stations) - n_errors, "stations successfully imputed.\n")
write_rds(errors, "data/ghcnd/imputation_errors.rds")
