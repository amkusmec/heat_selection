library(tidyverse)
library(abind)
source("src/bc_sum.R")


basis <- c("CUB", "STEP")
TEMP_eval <- -2:41

# Any regression dataframe will do; they only differ in the basis expansions
reg_dat <- read_rds("data/regression_data/reg_dat_CUB.rds")
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

y_idx <- years_m$Year >= 1975

sel <- map_df(basis, function(b) {
  map_df(c(TRUE, FALSE), function(yr) {
    cat(b, yr, "\n")
    rc <- paste0("data/regression_results2/", b, "_results/", 
                 b, "_temperature_random.rds") %>% 
      read_rds()
    
    if (yr) {
      rc <- rc[, y_idx, ]
      m_idx <- y_idx
    } else {
      rc <- rc[, !y_idx, ]
      m_idx <- !y_idx
    }
    
    cc <- sapply(1:(dim(rc)[1]), function(i) {
      sapply(1:(dim(rc)[3]), function(j) {
        tryCatch({
          lm(rc[i, , j] ~ 1 + years_m$Year[m_idx], weights = years_m[[j + 2]][m_idx]) %>%
            coef() %>% 
            `[`(2) %>% 
            unname()
        }, error = function(e) as.numeric(NA))
      })
    })
    colnames(cc) <- TEMP_eval
    
    as_tibble(cc) %>% 
      mutate(Bootstrap = 1:n()) %>% 
      pivot_longer(-Bootstrap, names_to = "Temperature", values_to = "Slope") %>% 
      mutate(Basis = b, 
             Group = if_else(yr, "post-1975", "pre-1975")) %>% 
      select(Basis, Group, everything())
  })
})

gc()

sel_post <- map_df(c(0.95, 0.9), function(a) {
      sel %>% 
        group_by(Basis, Group, Temperature) %>% 
        summarise(S = enframe(bc_sum(Slope, alpha = a)), 
                  .groups = "drop") %>% 
        unnest(c(S)) %>% 
        pivot_wider(names_from = "name", values_from = "value") %>% 
        mutate(Alpha = a)
    }) %>% 
  mutate(Alpha = paste0("bold(alpha==", Alpha, ")"), 
         Temperature = as.integer(Temperature))

sel_post <- sel_post %>% 
  filter(Temperature != -1) %>% 
  filter(!(Basis == "STEP" & Temperature %in% 0:1))

ggplot(filter(sel_post, Temperature > -2, !str_detect(Alpha, "5")), 
       aes(x = Temperature)) + theme_classic() + 
  geom_line(aes(y = Lower, colour = Group), linetype = 2) + 
  geom_line(aes(y = Upper, colour = Group), linetype = 2) + 
  geom_line(aes(y = Mean, colour = Group), linetype = 1) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "black") + 
  facet_wrap(~ Basis, nrow = 1, scales = "free_y", 
             labeller = as_labeller(c("CUB" = "CUBIC", "STEP" = "CONSTANT"))) + 
  scale_colour_manual(values = c("pre-1975" = "blue", "post-1975" = "red")) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), 
       y = "% yield/h exposure/yr", colour = "") + 
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"))
ggsave("figures/subset_selection.png", width = 8, height = 3.5, units = "in")
