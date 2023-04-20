library(tidyverse)

weather <- read_csv("data/strict/temperature_noI.csv") %>% 
  select(-Fixed) %>% 
  group_by(Year, State, County) %>% 
  mutate(Variable = Variable/sum(Variable)) %>% 
  filter(Temperature >= 30) %>% 
  summarise(Hours = sum(Variable), 
            .groups = "drop") %>% 
  group_by(State, County) %>% 
  summarise(Hours = mean(Hours), 
            .groups = "drop") %>% 
  mutate(State = str_to_lower(State))

states <- map_data("state", region = c("illinois", "iowa", "nebraska", "kansas"))
counties <- sf::read_sf("data/prism/county_outlines/tl_2017_us_county.shp") %>% 
  filter(COUNTYNS %in% weather$County) %>% 
  mutate(NAME = str_to_upper(NAME) %>% 
           str_replace("DEKALB", "DE KALB") %>% 
           str_replace("O'BRIEN", "OBRIEN") %>% 
           str_replace("ST. CLAIR", "ST CLAIR"))
county_df <- map_data("county") %>% 
  filter(region %in% c("illinois", "iowa", "nebraska", "kansas")) %>% 
  mutate(subregion = str_to_upper(subregion)) %>% 
  filter(subregion %in% counties$NAME) %>% 
  inner_join(select(counties, NAME, COUNTYNS), by = c("subregion" = "NAME")) %>% 
  inner_join(weather, by = c("region" = "State", "COUNTYNS" = "County"))

fill_sc <- colorRampPalette(c("#ffffcc", "#fd8d3c", "#800026"))
ggplot() + theme_classic() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Hours), 
               data = county_df, colour = "black", size = 0.25) + 
  geom_polygon(aes(x = long, y = lat, group = region), data = states, 
               colour = "black", fill = "white", size = 0.75, alpha = 0) + 
  scale_x_continuous(breaks = seq(-85, -105, -2.5), 
                     labels = parse(text = paste0(seq(85, 105, 2.5), "*degree", "*W"))) + 
  scale_y_continuous(breaks = seq(36, 47, 2), 
                     labels = parse(text = paste0(seq(36, 47, 2), "*degree", "*N"))) + 
  scale_fill_stepsn(colours = fill_sc(6), n.breaks = 6, show.limits = TRUE, 
                    labels = scales::label_percent(accuracy = 1)) +  
  labs(x = "", y = "", fill = "Avg. % exposure\nto heat stress") + 
  theme(legend.title = element_text(size = 10, face = "bold"))
ggsave("figures/SF04_county_exposure.png", width = 7, height = 3, units = "in")
