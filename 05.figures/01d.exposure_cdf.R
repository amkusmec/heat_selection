# Needs to be run after 01c.average_exposure.R

# Illustrate the cumulative distribution functions of exposure and the average 
# proportion of the season exposed to stressful temperatures

cdf_avg <- avg_weather %>% 
  group_by(State) %>% 
  arrange(Temperature) %>% 
  mutate(Value = cumsum(Value)/sum(Value)) %>% 
  ungroup()

cdf30 <- cdf_avg %>% 
  filter(Temperature == 30)

p_cdf <- ggplot() + theme_classic() + 
  annotate("rect", xmin = -Inf, xmax = 8, ymin = -Inf, ymax = Inf, 
           fill = "#8DA0CB", alpha = 0.5) + 
  annotate("rect", xmin = 8, xmax = 30, ymin = -Inf, ymax = Inf, 
           fill = "#66C2A5", alpha = 0.5) + 
  annotate("rect", xmin = 30, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = "#FC8D62", alpha = 0.5) + 
  geom_line(aes(x = Temperature, y = Value, colour = State, 
                linetype = LT, size = State), cdf_avg) + 
  geom_point(aes(x = Temperature, y = Value, colour = State), cdf30, size = 2) + 
  geom_segment(aes(y = Value, yend = Value, colour = State), cdf30, 
               x = -Inf, xend = 30, linetype = 1) + 
  scale_colour_manual(values = state_sc) + 
  scale_size_manual(values = c("Average" = 1.7, "Illinois" = 1.2, "Iowa" = 1.2, 
                               "Kansas" = 1.2, "Nebraska" = 1.2)) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), 
                     breaks = seq(0, 1, 0.1)) + 
  guides(linetype = "none", size = "none", colour = "none") + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), colour = "", 
       y = "Cumulative distribution function") + 
  theme(axis.title = element_text(face = "bold"))
