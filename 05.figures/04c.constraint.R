
con_perc <- read_csv("data/regression_results2/constraint_variance.csv") %>% 
  filter(Basis == "CUB", SB <= 3) %>% 
  mutate(SB = paste0("SB", SB), 
         Label = paste0(SB, ", ", round(Mean, 3)*100, "%"))

con_func <- read_csv("data/regression_results2/constraint_functions.csv") %>% 
  filter(Basis == "CUB", SB %in% paste0("SB", 1:3))

p_con <- ggplot() + theme_classic() + 
  geom_line(aes(x = Temperature, y = Mean, colour = SB), con_func) + 
  geom_line(aes(x = Temperature, y = Lower, colour = SB), con_func, linetype = 2) + 
  geom_line(aes(x = Temperature, y = Upper, colour = SB), con_func, linetype = 2) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_label(aes(label = Label, fill = SB), con_perc, colour = "white", 
             fontface = "bold", x = 10, y = -0.35) + 
  scale_colour_brewer(type = "qual", palette = "Accent") + 
  scale_fill_brewer(type = "qual", palette = "Accent") + 
  facet_wrap(~ SB, nrow = 1) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), y = "Loading") + 
  guides(colour = "none", fill = "none") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"))
