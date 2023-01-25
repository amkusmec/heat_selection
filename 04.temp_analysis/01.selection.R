library(tidyverse)
library(abind)
source("src/bc_sum.R")


basis <- c("CUB", "STEP")
TEMP_eval <- -1:41

# Any regression dataframe will do; they only differ in the basis expansions
reg_dat <- read_rds("data/regression_data/reg_dat_LIN.rds")
boots <- read_rds("data/regression_data/block_bootstraps.rds")
years_m <- reg_dat %>% 
  mutate(Year = attr(Year, "scaled:scale")*Year + attr(Year, "scaled:center")) %>% 
  group_by(Variety) %>% 
  summarise(Year = min(Year), 
            .groups = "drop") %>% 
  arrange(Variety)
years <- map(1:ncol(boots), function(i) {
  reg_dat[boots[, i], ] %>% 
    count(Variety)
})

years_m <- reduce(years, full_join, by = "Variety", .init = years_m)


sel <- map_df(basis, function(b) {
  cat(b, "\n")
  rc <- paste0("data/regression_results2/", b, "_results/", 
               b, "_temperature_random.rds") %>% 
    read_rds()
  
  cc <- sapply(1:(dim(rc)[1]), function(i) {
    sapply(1:(dim(rc)[3]), function(j) {
      lm(rc[i, , j] ~ 1 + years_m$Year, weights = years_m[[j + 2]]) %>%
        coef() %>% 
        `[`(2) %>% 
        unname()
    })
  })
  colnames(cc) <- c("Intercept", TEMP_eval)
  
  as_tibble(cc) %>% 
    mutate(Bootstrap = 1:n()) %>% 
    pivot_longer(-Bootstrap, names_to = "Temperature", values_to = "Slope") %>% 
    mutate(Basis = b) %>% 
    select(Basis, everything())
})

gc()

sel_post <- sel %>% 
  group_by(Basis) %>% 
  group_modify(~ {
      tnames <- unique(.x$Temperature)
      temp <- .x %>% 
        pivot_wider(names_from = "Bootstrap", values_from = "Slope") %>% 
        select(-Temperature) %>% 
        as.matrix()
      apply(temp, 1, function(x) c("Full" = x[1], bc_sum(x))) %>% 
        t() %>% 
        as_tibble() %>% 
        mutate(Temperature = tnames) %>% 
        select(Temperature, everything())
    }) %>% 
  ungroup()

sel_post %>% 
  filter(Temperature == "Intercept") %>% 
  ggplot(aes(x = Basis)) + theme_bw() + 
    geom_linerange(aes(ymin = Lower, ymax = Upper)) + 
    geom_point(aes(y = Mean), shape = 2, size = 3) + 
    geom_point(aes(y = Full.1), shape = 19, size = 3, colour = "red") + 
    scale_y_continuous(labels = scales::percent) + 
    labs(x = "Basis", y = "% yield/yr")

sel_post %>% 
  filter(Temperature != "Intercept") %>% 
  mutate(Temperature = as.integer(Temperature)) %>% 
  filter(Temperature >= 0) %>% 
  ggplot(aes(x = Temperature)) + theme_bw() + 
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "grey80", alpha = 0.5) + 
    geom_line(aes(y = Mean)) + 
    geom_hline(yintercept = 0, linetype = 2, colour = "red") + 
    facet_wrap(~ Basis, scales = "free_y")

write_csv(sel, "data/regression_results2/selection_full_posterior.csv")
write_csv(sel_post, "data/regression_results2/selection_sum_posterior.csv")
