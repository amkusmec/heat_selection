# Illustrate the geographic distribution and temporal replication of trial sites
# paired with recent yield data from USDA

# Data for trial site replication
trials <- read_csv("data/strict/trials_noI.csv")
trial_counts <- trials %>%
  group_by(County) %>%
  summarise(Longitude = unique(Longitude), 
            Latitude = unique(Latitude), 
            n = n(), 
            .groups = "drop")

# State outlines
state_names <- c("illinois", "iowa", "nebraska", "kansas")
states <- map_data("state", region = state_names)

# Pair county outlines with yields
county_yields <- read_csv("data/USDA_county_yield_2021.csv") %>%
  select(State, County, Value) %>%
  mutate(across(c(State, County), str_to_lower))
county_df <- map_data("county") %>% 
  semi_join(states, by = "region") %>% 
  left_join(county_yields, by = c("region" = "State", "subregion" = "County"))

p_map <- ggplot() + theme_classic() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Value), 
               data = county_df, colour = "black", size = 0.25) + 
  geom_polygon(aes(x = long, y = lat, group = region), data = states, 
               colour = "black", fill = "white", size = 0.75, alpha = 0) + 
  # scale_fill_distiller(type = "div", palette = "RdYlGn", direction = 1, 
  #                      na.value = "transparent", guide = "colorbar") + 
  scale_fill_steps2(low = "#d73027", mid = "#ffffbf", 
                    high = "#1a9850", midpoint = 155, 
                    na.value = "transparent", show.limits = TRUE, n.breaks = 8) + 
  geom_point(aes(x = Longitude, y = Latitude),
             trial_counts, shape = 19, size = 3) +
  geom_point(aes(x = Longitude, y = Latitude, colour = n), 
             trial_counts, shape = 19, size = 2.6) + 
  scale_colour_distiller(n.breaks = 6, type = "seq", palette = "PuBu", direction = 1, 
                         guide = "legend") + 
  scale_x_continuous(breaks = seq(-85, -105, -2.5), 
                     labels = parse(text = paste0(seq(85, 105, 2.5), "*degree", "*W"))) + 
  scale_y_continuous(breaks = seq(36, 47, 2), 
                     labels = parse(text = paste0(seq(36, 47, 2), "*degree", "*N"))) +
  labs(x = "", y = "", colour = "# Trials", size = "# Trials", 
       fill = "2021 yield (bu/ac)") + 
  theme(legend.title = element_text(face = "bold"))
