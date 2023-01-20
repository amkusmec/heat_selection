library(tidyverse)
library(grid)
library(gridExtra)
source("src/bc_sum.R")


trials <- read_csv("data/strict/trials_noI.csv") %>% 
  distinct(County, .keep_all = TRUE) %>% 
  arrange(Latitude, Longitude)

basis <- c("CUB", "STEP")
ceff <- map_df(basis, function(b) {
  paste0("data/regression_results2/", b, "_results/", b, "_county_random.rds") %>% 
    read_rds() %>% 
    exp() %>% 
    `-`(1) %>% 
    apply(2, bc_sum) %>% 
    t() %>% 
    as_tibble(rownames = "County") %>% 
    arrange(Mean) %>% 
    mutate(Basis = b, 
           County = str_remove(County, "County")) %>% 
    inner_join(select(trials, County:Latitude), by = "County") %>% 
    arrange(Latitude, Longitude) %>% 
    mutate(Idx = 1:n()) %>% 
    select(Basis, everything())
})

p_comp <- ceff %>% 
  select(Basis:Mean, Idx) %>% 
  pivot_wider(names_from = "Basis", values_from = "Mean") %>% 
  ggplot() + theme_classic() + 
    geom_point(aes(x = CUB, y = STEP)) + 
    geom_abline(intercept = 0, slope = 1, linetype = 2, colour = "red") + 
    scale_x_continuous(labels = scales::label_percent()) + 
    scale_y_continuous(labels = scales::label_percent()) + 
    labs(x = "Cubic B-splines", y = "Constant B-splines", tag = "B") + 
    theme(axis.title = element_text(face = "bold"))

ceff2 <- ceff %>% 
  filter(Basis == "CUB") %>% 
  select(County:Mean) %>% 
  inner_join(select(trials, State:County), by = "County") %>% 
  mutate(State = str_to_lower(State))
states <- map_data("state", region = c("illinois", "iowa", "nebraska", "kansas"))
counties <- sf::read_sf("data/prism/county_outlines/tl_2017_us_county.shp") %>% 
  filter(COUNTYNS %in% ceff$County) %>% 
  mutate(NAME = str_to_upper(NAME) %>% 
           str_replace("DEKALB", "DE KALB") %>% 
           str_replace("O'BRIEN", "OBRIEN") %>% 
           str_replace("ST. CLAIR", "ST CLAIR"))
county_df <- map_data("county") %>% 
  filter(region %in% c("illinois", "iowa", "nebraska", "kansas")) %>% 
  mutate(subregion = str_to_upper(subregion)) %>% 
  filter(subregion %in% counties$NAME) %>% 
  inner_join(select(counties, NAME, COUNTYNS), by = c("subregion" = "NAME")) %>% 
  inner_join(ceff2, by = c("region" = "State", "COUNTYNS" = "County"))

p_map <- ggplot() + theme_classic() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Mean), 
               data = county_df, colour = "black", size = 0.25) + 
  geom_polygon(aes(x = long, y = lat, group = region), data = states, 
               colour = "black", fill = "white", size = 0.75, alpha = 0) + 
  scale_x_continuous(breaks = seq(-85, -105, -2.5), 
                     labels = parse(text = paste0(seq(85, 105, 2.5), "*degree", "*W"))) + 
  scale_y_continuous(breaks = seq(36, 47, 2), 
                     labels = parse(text = paste0(seq(36, 47, 2), "*degree", "*N"))) + 
  scale_fill_steps2(low = "#d73027", mid = "#ffffbf", high = "#1a9850", 
                    midpoint = 0, show.limits = TRUE, n.breaks = 8, 
                    labels = scales::label_percent()) +  
  labs(x = "", y = "", fill = expression(bold(Delta*"% yield")), tag = "A")


gp <- arrangeGrob(ggplotGrob(p_map), ggplotGrob(p_comp), 
                  layout_matrix = matrix(c(1, 1, 2), nrow = 1))
ggsave("figures/revision1/county_effects.png", gp, 
       width = 9, height = 3, units = "in")
