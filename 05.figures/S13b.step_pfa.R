# Plot principal functions analysis results for constant B-spline basis

pf_perc <- read_csv("data/regression_results2/pfa_variance.csv") %>% 
  filter(Basis == "STEP", PF <= 3) %>% 
  mutate(PF = paste0("PF", PF), 
         Label = paste0(PF, ", ", round(Mean, 3)*100, "%"))

pf_func <- read_csv("data/regression_results2/pfa_functions.csv") %>% 
  filter(Basis == "STEP", PF %in% paste0("PF", 1:3))

p_pfa <- ggplot() + theme_classic() + 
  geom_step(aes(x = Temperature, y = Mean, colour = PF), pf_func) + 
  geom_step(aes(x = Temperature, y = Lower, colour = PF), pf_func, linetype = 2) + 
  geom_step(aes(x = Temperature, y = Upper, colour = PF), pf_func, linetype = 2) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_label(aes(label = Label, fill = PF), pf_perc, colour = "white", 
             fontface = "bold", x = c(15, 15, 26), y = c(-0.35, 0.4, 0.45)) + 
  scale_colour_brewer(type = "qual", palette = "Set2") + 
  scale_fill_brewer(type = "qual", palette = "Set2") + 
  facet_wrap(~ PF, nrow = 1) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), y = "Loading") + 
  guides(colour = "none", fill = "none") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"))
