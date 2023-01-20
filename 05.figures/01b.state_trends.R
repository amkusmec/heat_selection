# Illustrate (a) distributions of trial results by year, (b) state yield trends, 
# and (c) concordance between the two

yield <- read_csv("data/strict/yield_noI.csv") %>%
  mutate(State = recode(State, "ILLINOIS" = "Illinois (IL)", "IOWA" = "Iowa (IA)", 
                        "KANSAS" = "Kansas (KS)", "NEBRASKA" = "Nebraska (NE)"))

st_yield <- read_csv("data/USDA_state_yields.csv") %>% 
  filter(State != "WISCONSIN") %>% 
  mutate(State = recode(State, "ILLINOIS" = "Illinois (IL)", "IOWA" = "Iowa (IA)", 
                        "KANSAS" = "Kansas (KS)", "NEBRASKA" = "Nebraska (NE)"))

cors <- yield %>% 
  group_by(State, Year) %>% 
  summarise(Mean = mean(Yield), 
            .groups = "drop") %>% 
  inner_join(st_yield, by = c("State", "Year")) %>%
  group_by(State) %>%
  summarise(R = cor(Mean, Value) %>% 
              `^`(2) %>% 
              round(2), 
            .groups = "drop")

p_trends <- ggplot() + theme_classic() +
  geom_boxplot(aes(x = Year, group = Year, y = Yield), yield, 
               outlier.colour = "grey50", outlier.shape = 1, outlier.size = 0.4) + 
  geom_line(aes(y = Value, x = Year, colour = State), st_yield, size = 1) + 
  geom_label(aes(label = paste("r^2==", R, sep = "")), cors, x = 1945, 
             y = 225, parse = TRUE) + 
  facet_wrap(~ State) + guides(colour = "none") + 
  scale_y_continuous(breaks = seq(0, 300, 25), labels = seq(0, 300, 25)) + 
  scale_x_continuous(breaks = seq(1930, 2020, 10), labels = seq(1930, 2020, 10)) + 
  scale_colour_brewer(type = "qual", palette = "Set1") + 
  labs(x = "", y = "Yield (bu/ac)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"))
