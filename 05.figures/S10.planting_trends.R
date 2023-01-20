library(tidyverse)
library(lubridate)

trials <- read_csv("data/strict/trials_noI.csv") %>% 
  mutate(YDAY_P = yday(Planted))

trial_models <- trials %>% 
  nest(data = !State) %>% 
  rowwise() %>% 
  mutate(Model = list(lm(YDAY_P ~ Year, data = data))) %>% 
  ungroup() %>% 
  mutate(YearP = map_dbl(Model, function(m) summary(m)$coefficients[2, 4]), 
         YearP = if_else(YearP < 2.2e-16, "p < 2.2%*%10^-16", 
                         format(YearP, digits = 2) %>% str_replace("e", "%*%10^")))

trials2 <- trials %>% 
  filter(Year >= 1981)
trial_models2 <- trials2 %>% 
  nest(data = !State) %>% 
  rowwise() %>% 
  mutate(Model = list(lm(YDAY_P ~ Year, data = data))) %>% 
  ungroup() %>% 
  mutate(YearP = map_dbl(Model, function(m) summary(m)$coefficients[2, 4]), 
         YearP = if_else(YearP < 2.2e-16, "p < 2.2%*%10^-16", 
                         format(YearP, digits = 2) %>% str_replace("e", "%*%10^") %>% 
                           map_chr(function(x) paste0("p == ", x))))

ggplot() + theme_classic() + 
  geom_boxplot(aes(x = Year, y = YDAY_P, group = Year), trials, 
               outlier.colour = "red", outlier.shape = 1, outlier.size = 0.5) +
  geom_smooth(aes(x = Year, y = YDAY_P), trials, method = "lm", 
              se = TRUE, colour = "blue", linetype = 2, size = 0.5, 
              fill = "grey80", alpha = 0.5) + 
  geom_text(aes(label = YearP), trial_models, x = 1955, y = 175, 
            parse = TRUE, size = 3, colour = "blue") + 
  geom_smooth(aes(x = Year, y = YDAY_P), trials2, method = "lm", 
              se = TRUE, colour = "brown", linetype = 2, size = 0.5, 
              fill = "grey80", alpha = 0.5) + 
  geom_text(aes(label = YearP), trial_models2, x = 2000, y = 175, 
            parse = TRUE, size = 3, colour = "brown") + 
  # facet_grid(. ~ State, labeller = as_labeller(str_to_title)) + 
  facet_wrap(~ State, labeller = as_labeller(str_to_title)) + 
  labs(x = "", y = "Julian day") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"))
ggsave("figures/revision1/planting_trends.png", width = 8, height = 5, units = "in")
