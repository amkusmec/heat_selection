library(tidyverse)


temp <- read_csv("data/strict/temperature_noI.csv", col_types = cols()) %>% 
  rename(Time = Variable) %>% 
  select(-Fixed) %>% 
  mutate(Time = Time/24) # Convert back to days

cty_qc <- temp %>% 
  distinct(County, Year) %>% 
  count(County) %>% 
  filter(n >= 4) %>% 
  pull(County)

temp_time <- temp %>% 
  filter(County %in% cty_qc) %>% 
  nest(data = c(Year, Time)) %>% 
  rowwise() %>% 
  mutate(Model = list(lm(Time ~ 1 + Year, data = data)), 
         A = coef(Model)[1] %>% unname(), 
         B = coef(Model)[2] %>% unname(), 
         P = summary(Model)$coefficients[2, 4]) %>% 
  ungroup() %>% 
  select(-data, -Model) %>% 
  mutate(Sig = P <= 0.05, 
         Sign = sign(B))

temp %>% distinct(State, County) %>% count(State)
temp_time %>% distinct(State, County) %>% count(State)

write_csv(temp_time, "data/strict/exposure_trends_all.csv")
