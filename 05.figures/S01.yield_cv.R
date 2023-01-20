library(tidyverse)

yield <- read_rds("data/strict/regression_data.rds")

v <- yield %>% 
  group_by(Year, State) %>% 
  summarise(Mean = mean(Yield), 
            SD = sd(Yield), 
            .groups = "drop") %>% 
  mutate(CV = SD/Mean, 
         State = recode_factor(State, "ILLINOIS" = "Illinois (IL)", 
                               "IOWA" = "Iowa (IA)", 
                               "KANSAS" = "Kansas (KS)", 
                               "NEBRASKA" = "Nebraska (NE)")) %>% 
  pivot_longer(Mean:CV, names_to = "Variable", values_to = "Value") %>% 
  mutate(Variable = recode_factor(Variable, "Mean" = "Mean yield\n(bu/a)", 
                                  "SD" = "Standard deviation\n(bu/a)"))

m <- v %>% 
  nest_by(State, Variable) %>% 
  mutate(Model = list(lm(Value ~ Year, data = data)), 
         Intercept = list(round(coef(Model)[1], 0)), 
         Slope = list(round(coef(Model)[2], 3)), 
         Form = list(paste0("y=", Intercept, "+", Slope, "t"))) %>% 
  select(State:Variable, Form) %>% 
  ungroup() %>% 
  unnest(Form) %>% 
  inner_join(group_by(v, Variable) %>% summarise(Value = 0.9*max(Value)), 
             by = "Variable")

ggplot(v) + theme_classic() + 
  geom_point(aes(x = Year, y = Value), size = 0.8) + 
  geom_smooth(aes(x = Year, y = Value, colour = State), method = "lm") + 
  geom_text(aes(y = Value, label = Form), m, x = 1960, size = 3) + 
  facet_grid(Variable ~ State, scales = "free_y") + 
  scale_x_continuous(breaks = seq(1930, 2020, 10), labels = seq(1930, 2020, 10)) +
  scale_colour_brewer(type = "qual", palette = "Set1") + 
  labs(x = "", y = "") + guides(colour = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text = element_text(face = "bold"))
ggsave("figures/revision1/yield_cv.png", width = 8, height = 5, units = "in")

