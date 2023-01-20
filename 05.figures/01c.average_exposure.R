# Illustrate temperature domains and average exposure

state_sc <- c("Average" = "black", "Illinois" = "#E41A1C", "Iowa" = "#377EB8", 
              "Kansas" = "#4DAF4A", "Nebraska" = "#984EA3")

avg_weather <- read_csv("data/strict/temperature_noI.csv") %>%
  group_by(State, Temperature) %>%
  summarise(Value = mean(Variable), 
            .groups = "drop") %>% 
  mutate(State = str_to_title(State))
avg_weather <- bind_rows(avg_weather, 
                         read_csv("data/strict/temperature_noI.csv") %>%
                           group_by(Temperature) %>%
                           summarise(Value = mean(Variable), 
                                     .groups = "drop") %>% 
                           mutate(State = "Average") %>% 
                           select(State, everything())) %>% 
  mutate(LT = if_else(State == "Average", "1", "2"))

p_exposure <- ggplot(avg_weather) + theme_classic() + 
  annotate("rect", xmin = -Inf, xmax = 8, ymin = -Inf, ymax = Inf, 
           fill = "#8DA0CB", alpha = 0.5) + 
  annotate("rect", xmin = 8, xmax = 30, ymin = -Inf, ymax = Inf, 
           fill = "#66C2A5", alpha = 0.5) + 
  annotate("rect", xmin = 30, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = "#FC8D62", alpha = 0.5) + 
  geom_line(aes(x = Temperature, y = Value, colour = State, 
                size = State, linetype = LT)) + 
  scale_colour_manual(values = state_sc) + 
  scale_size_manual(values = c("Average" = 1.7, "Illinois" = 1.2, "Iowa" = 1.2, 
                               "Kansas" = 1.2, "Nebraska" = 1.2)) + 
  guides(linetype = "none", size = "none", colour = "none") + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), y = "Average exposure (h)", 
       colour = "") + 
  theme(axis.title = element_text(face = "bold"))
