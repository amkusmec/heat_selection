

sb_var <- read_csv("data/regression_results2/simple_basis_variance.csv") %>% 
  filter(Basis == "CUB", SB <= 3) %>% 
  mutate(SB = paste0("SB", SB), 
         Label = paste0(SB, ", ", round(Mean, 3)*100, "%"))
sb_func <- read_csv("data/regression_results2/simple_basis_functions.csv") %>% 
  filter(Basis == "CUB", SB %in% paste0("SB", 1:3))

p_sb <- ggplot() + theme_classic() + 
  geom_line(aes(x = Temperature, y = Mean, colour = SB), sb_func, size = 1.25) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_label(aes(label = Label, fill = SB), sb_var, colour = "white", 
             fontface = "bold", x = 27, y = c(-0.1, -0.15, -0.2)) + 
  scale_colour_brewer(type = "qual", palette = "Set1") + 
  scale_fill_brewer(type = "qual", palette = "Set1") + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), y = "Loading") + 
  guides(colour = "none", fill = "none") + 
  theme(axis.title = element_text(face = "bold"))
