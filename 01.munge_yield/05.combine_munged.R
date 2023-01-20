library(tidyverse)


d <- c("data/trial_data/munged_IL.csv", "data/trial_data/munged_KS.csv", 
       "data/trial_data/munged_NE.csv", "data/trial_data/munged_IA.csv") %>%
  map_df(read_csv) %>%
  mutate(Brand = if_else(Brand == "PAG", "PFISTER", Brand)) %>%
  filter(!str_detect(Variety, "DENT"), Brand != "H122W WHITE", 
         !str_detect(Brand, "PRIDE"), Brand != "AES") %>%
  filter(!(is.na(Region) & is.na(County) & is.na(Town))) %>%
  mutate(Variety = str_remove_all(Variety, "\\(EXP\\.\\)") %>%
           str_remove_all(., "[:punct:]") %>%
           str_remove_all(., "SX$") %>%
           str_remove_all(., "EX$") %>%
           str_remove_all(., "EXP$") %>%
           str_remove_all(., " "), 
         Variety = if_else(str_detect(Variety, Brand), 
                           str_remove(Variety, Brand), Variety)) %>%
  filter(Variety != "")

write_csv(d, "data/trial_data/munged_trials_all.csv")
