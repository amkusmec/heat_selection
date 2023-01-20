library(tidyverse)
source("src/bc_sum.R")

basis <- c("CUB", "STEP")
years <- read_rds("data/regression_data/reg_dat_CUB.rds") %>% 
  distinct(State, Year) %>% 
  arrange(State, Year) %>% 
  mutate(Year = drop(Year))

times <- map_df(basis, function(b) {
      cc <- paste0("data/regression_results2/", b, "_results/", b, "_year_random.rds") %>% 
        read_rds()
      
      years %>% 
        group_by(State) %>% 
        group_modify(~ {
            poly_x <- poly(.x$Year, degree = 2, simple = TRUE)
            cn <- which(str_detect(colnames(cc), .y$State))
            temp <- cc[, cn] %*% t(poly_x)
            
            apply(temp, 2, bc_sum) %>% 
              t() %>% 
              as_tibble() %>% 
              mutate(Year = attr(years$Year, "scaled:scale")*.x$Year + attr(years$Year, "scaled:center")) %>% 
              select(Year, everything())
          }) %>% 
        ungroup() %>% 
        mutate(Basis = b) %>% 
        select(Basis, everything())
    }) %>%
  mutate(State = recode(State, "ILLINOIS" = "Illinois (IL)", "IOWA" = "Iowa (IA)", 
                        "KANSAS" = "Kansas (KS)", "NEBRASKA" = "Nebraska (NE)")) %>% 
  mutate(across(Mean:Upper, ~ .x/attr(years$Year, "scaled:scale")))

ggplot(times, aes(x = Year)) + theme_classic() + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = State), alpha = 0.25) + 
  geom_line(aes(y = Mean, colour = State), size = 0.75) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "black") + 
  # geom_vline(xintercept = 1981) + 
  facet_grid(State ~ Basis, scales = "free_y", 
             labeller = labeller(.cols = c("CUB" = "CUBIC", "STEP" = "CONSTANT"))) +
  labs(x = "", y = "% yield/yr") + 
  scale_y_continuous(labels = scales::label_percent()) + 
  scale_x_continuous(breaks = seq(1930, 2020, 20), labels = seq(1930, 2020, 20)) + 
  scale_colour_brewer(type = "qual", palette = "Set1") +
  scale_fill_brewer(type = "qual", palette = "Set1") + 
  guides(colour = "none", fill = "none") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"))
ggsave("figures/revision1/time_trends.png", width = 6, height = 4.5, units = "in")
