library(tidyverse)
library(lubridate)


trials <- read_csv("data/strict/trials_noI.csv", col_types = cols()) %>% 
  mutate(PDAY = yday(Planted), 
         HDAY = yday(Harvested), 
         GSL = as.integer(HDAY - PDAY))

tr_qc <- trials %>% 
  count(County) %>% 
  filter(n >= 4) %>% 
  pull(County)

trials_time <- trials %>% 
  select(Year:County, PDAY:GSL) %>% 
  filter(County %in% tr_qc) %>% 
  pivot_longer(PDAY:GSL, names_to = "Variable", values_to = "Value") %>% 
  nest(data = c(Year, Value)) %>% 
  rowwise() %>% 
  mutate(Model = list(lm(Value ~ 1 + Year, data = data)), 
         A = coef(Model)[1] %>% unname(), 
         B = coef(Model)[2] %>% unname(), 
         P = summary(Model)$coefficients[2, 4]) %>% 
  ungroup() %>% 
  select(-data, -Model) %>% 
  mutate(Sig = P <= 0.05, 
         Sign = sign(B))

write_csv(trials_time, "data/strict/date_trends_all.csv")
