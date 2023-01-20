library(tidyverse)
library(purrrlyr)
library(parallel)


files <- list.files("data/prism/dailys", "*\\.csv", full.names = TRUE)

res <- mclapply(files, function(f) {
  cat("File", f, "\n")
  temp <- read_csv(f, col_types = "iiinnn") %>%
    by_row(function(r) {
        if (is.na(r$TMIN[1]) | is.na(r$TMAX[1])) {
          tibble(Degree = 0, Time = as.numeric(NA))
        } else if (floor(r$TMIN[1]) == floor(r$TMAX[1])) {
          tibble(Degree = floor(r$TMIN[1]), 
                 Time = 1)
        } else {
          M <- (r$TMAX[1] + r$TMIN[1])/2
          W <- (r$TMAX[1] - r$TMIN[1])/2
          
          temp_grid <- seq(ceiling(r$TMIN[1]), floor(r$TMAX[1]))
          time_grid <- asin((temp_grid - M)/W)
          hour_grid <- c(0, 12*time_grid/pi + 6, 12)
          
          tibble(Degree = c(min(temp_grid) - 1, temp_grid), 
                 Time = 2*diff(hour_grid)/24)
        }
      }, .to = "Distribution") %>%
    unnest(Distribution) %>%
    arrange(Degree) %>%
    mutate(Degree = make.names(Degree)) %>%
    pivot_wider(names_from = "Degree", values_from = "Time", values_fill = list("Time" = 0)) %>%
    arrange(YEAR, MONTH, DAY)
  
  write_csv(temp, str_replace_all(f, "daily", "distribution"))
  TRUE
}, mc.cores = detectCores()/2 - 1)
