library(tidyverse)
library(lubridate)
library(raster)
library(VGAM)
library(parallel)

options(readr.show_progress = FALSE)


# Link cells to the appropriate seasons
imputed_files <- list.files("data/prism/dailys", "*", full.names = FALSE) %>% 
  str_split("_") %>% 
  sapply(`[[`, 2) %>% 
  as.integer()
station_map <- read_rds("data/prism/grid_to_station_mapping.rds") %>% 
  filter(!(Cell %in% imputed_files))

prism_files <- list.files("data/prism/subset", "*\\.csv", full.names = TRUE)


# Function to predict daily PRISM values ----------------------------------
predict_prism <- function(i) {
  options(readr.show_progress = FALSE)
  
  file_name <- paste0("data/prism/dailys/PRISM_", station_map$Cell[i], "_daily.csv")
  
  pred <- map(c("TMAX", "TMIN", "PRCP"), function(v) {
    # Load the appropriate daily weather station data
    weather <- pmap_dfr(list(Year = station_map$STATIONS[[i]]$YEAR, 
                             Station = station_map$STATIONS[[i]][[v]]), 
                        function(Year, Station) {
                          map_df(Station, function(s) {
                            read_csv(paste0("data/ghcnd/imputed/", s, "_impute.csv"), 
                                     col_types = "ciiinnni", progress = FALSE) %>%
                              filter(YEAR == Year, MONTH <= 9) %>%
                              dplyr::select(ID:DAY, contains(v))
                          })
                        }) %>%
      group_by(YEAR) %>%
      group_modify(~ {
          temp <- pivot_wider(.x, names_from = "ID", values_from = all_of(v)) %>%
            dplyr::select(MONTH, DAY, everything())
          names(temp)[-c(1, 2)] <- paste0("X", 1:7)
          temp
        }) %>%
      ungroup() %>%
      mutate(MONTH = as.character(MONTH))
    
    # Summarize by year and month
    weather_month <- weather %>%
      dplyr::select(-DAY) %>%
      group_by(YEAR, MONTH) %>%
      summarise(across(X1:X7, ~ mean(.x, na.rm = TRUE)), 
                .groups = "drop")
    
    # Load the PRISM monthly weather data
    v2 <- if_else(v == "PRCP", "ppt", str_to_lower(v))
    prism <- map_df(station_map$STATIONS[[i]]$YEAR, function(Year) {
                       token <- paste0(Year, paste0("0", 1:9))
                       idx <- sapply(token, function(tt) str_detect(prism_files, tt)) %>%
                         apply(., 1, any)
                       idx <- which(idx & str_detect(prism_files, v2))
                       
                       map_df(idx, function(j) {
                         read_csv(prism_files[j], col_types = "ciicnnnn") %>%
                           filter(Cell == station_map$Cell[i]) %>%
                           dplyr::select(Year, Month, Cell, Value)
                       })
                     }) %>%
      mutate(Month = as.character(Month))
    
    # Convert total monthly precipitation to average
    if (v2 == "ppt") {
      prism <- prism %>%
        mutate(DM = paste(Year, Month, sep = "-") %>%
                 parse_date_time(., orders = "Ym") %>%
                 days_in_month() %>%
                 unname(), 
               Value = Value/DM) %>%
        dplyr::select(-DM)
    }
    
    months <- unique(weather_month$MONTH)
    mm <- inner_join(weather_month, prism, by = c("YEAR" = "Year", "MONTH" = "Month")) %>%
      dplyr::select(-Cell)
    
    mm_miss <- filter(mm, is.na(Value))
    mm_train <- filter(mm, !is.na(Value))
    
    idx_miss <- which(weather$YEAR %in% mm_miss$YEAR & 
                        weather$MONTH %in% mm_miss$MONTH)
    
    if (nrow(mm_train) == 0) {
      weather %>%
        dplyr::select(YEAR:DAY) %>%
        mutate(!! v := as.numeric(NA))
    } else {
      # Check for rank deficiencies in the model matrix
      mock_model <- model.matrix(Value ~ . - YEAR, data = mm_train)
      rank_check <- caret::findLinearCombos(mock_model)
      
      if (!is.null(rank_check$remove)) {
        mm_train <- mm_train[, -which(names(mm_train) %in% colnames(mock_model)[rank_check$remove])]
      }
      
      # Fit models
      if (v %in% c("TMAX", "TMIN")) {
        # Normal OLS for temperature variables
        fit <- lm(Value ~ . - YEAR, data = mm_train)
        
        # Predict daily values
        if (length(idx_miss) > 0) {
          bind_rows(weather %>%
                      slice(-idx_miss) %>%
                      mutate(!! v := predict(fit, weather[-idx_miss, ]) %>% drop()), 
                    weather %>%
                      slice(idx_miss) %>%
                      mutate(!! v := as.numeric(NA))) %>%
            dplyr::select(YEAR:DAY) %>%
            arrange(YEAR, MONTH, DAY)
        } else {
          weather %>%
            dplyr::select(YEAR:DAY) %>%
            mutate(!! v := predict(fit, weather) %>% drop()) %>%
            arrange(YEAR, MONTH, DAY)
        }
      } else {
        # Tobit regression (i.e., left-censored at 0) for precipitation
        fit <- vglm(Value ~ . - YEAR, tobit(Lower = 0, type.fitted = "censored", 
                                            imethod = 3, zero = 2), 
                    data = mm_train)
        
        # Predict daily values
        if (length(idx_miss) > 0) {
          bind_rows(weather %>%
                      dplyr::select(YEAR:DAY) %>%
                      slice(-idx_miss) %>%
                      mutate(!! v := predictvglm(fit, weather[-idx_miss, ], type = "response", 
                                                 type.fitted = "censored") %>% 
                               drop()), 
                    weather %>%
                      dplyr::select(YEAR:DAY) %>%
                      slice(idx_miss) %>%
                      mutate(!! v := as.numeric(NA))) %>%
            arrange(YEAR, MONTH, DAY)
        } else {
          weather %>%
            dplyr::select(YEAR:DAY) %>%
            mutate(!! v := predictvglm(fit, weather, type = "response", 
                                       type.fitted = "censored") %>% 
                     drop()) %>%
            arrange(YEAR, MONTH, DAY)
        }
      }
    }
  }) %>%
    reduce(inner_join, by = c("YEAR", "MONTH", "DAY"))
  
  pred <- pred %>% 
    mutate(across(TMAX:PRCP, unname)) %>% 
    mutate(MONTH = as.integer(MONTH)) %>% 
    arrange(YEAR, MONTH, DAY)
  
  if (all(is.na(pred$TMAX)) & all(is.na(pred$TMIN)) & all(is.na(pred$PRCP))) {
    return(FALSE)
  } else {
    write_csv(pred, file_name)
    return(TRUE)
  }
}


# Process cells in parallel -----------------------------------------------
cl <- makeCluster(detectCores()/2 - 1, outfile = "impute_prism.log")
clusterEvalQ(cl, {
  library(tidyverse)
  library(lubridate)
  library(VGAM)
})
clusterExport(cl, list("station_map", "prism_files", "predict_prism"))

res <- parLapplyLB(cl, 1:nrow(station_map), function(x) {
  cat("Cell", x, "\n")
  temp <- tryCatch(predict_prism(x), 
                   error = function(e) e)
  list(names(station_map)[x], temp)
})

stopCluster(cl)
write_rds(res, "data/prism/imputation_errors.rds")


for (i in 1:nrow(station_map)) {
  cat(i, "\n")
  predict_prism(i)
}
