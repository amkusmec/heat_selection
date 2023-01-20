library(tidyverse)
library(lubridate)


mat <- read_csv("data/USDA_corn_progress.csv", col_types = cols(), 
                col_select = c(Year:`Week Ending`, State, `Data Item`, Value)) %>% 
  mutate(Period = str_remove(Period, "WEEK #") %>% as.integer(), 
         `Data Item` = word(`Data Item`, start = -1), 
         YDAY = yday(`Week Ending`))

mat_max <- mat %>% 
  filter(`Data Item` != "EMERGED") %>%
  group_by(Year, State, `Data Item`) %>% 
  arrange(desc(Value)) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  filter(Value > 80)

mat_models <- mat_max %>% 
  nest(data = !c(State, `Data Item`)) %>% 
  rowwise() %>% 
  mutate(Model = list(lm(YDAY ~ Year + Value, data = data))) %>% 
  ungroup() %>% 
  mutate(YearP = map_dbl(Model, function(m) summary(m)$coefficients[2, 4])) %>% 
  arrange(State, `Data Item`)

ggplot(mat_max) + theme_classic() + 
  geom_smooth(aes(x = Year, y = YDAY, 
                  group = `Data Item`, linetype = `Data Item`), 
              method = "lm", se = TRUE, colour = "black", size = 0.5, 
              fill = "grey80", alpha = 0.5) + 
  geom_point(aes(x = Year, y = YDAY, colour = Value, shape = `Data Item`)) + 
  geom_hline(yintercept = yday(ymd("2022-09-30")), 
             linetype = 2, colour = "grey40") + 
  geom_text(aes(y = JDAY, label = YearP), 
            mat_models %>% mutate(JDAY = rep(c(262.5, 247.5), times = 4), 
                                  YearP = paste0("p = ", round(YearP, 2))), 
            x = 1995) + 
  facet_grid(. ~ State, labeller = as_labeller(str_to_title)) + 
  scale_colour_steps(low = "purple", high = "green", show.limits = TRUE, 
                     labels = ~ paste0(.x, "%")) + 
  scale_shape_discrete(labels = str_to_title) + 
  scale_linetype_discrete(labels = str_to_title) + 
  labs(x = "", y = "Julian day", colour = "Progress", 
       linetype = "Stage", shape = "Stage") + 
  theme(axis.title = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"))
ggsave("figures/revision1/usda_maturity.png", width = 8, height = 3, units = "in")
