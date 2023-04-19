# Plot the genetic variance for responses to different temperatures

apply(vc[-1, -1, ], 3, diag) %>% 
  as_tibble() %>% 
  mutate(Temperature = 0:41) %>% 
  pivot_longer(-Temperature, names_to = "Bootstrap", values_to = "Variance") %>% 
  mutate(Bootstrap = str_remove(Bootstrap, "V") %>% as.integer()) %>% 
  write_csv("data/supplementary_tables/ST08.temperature_variance.csv")

vc_diag <- apply(vc[-1, -1, ], 3, diag) %>% 
  apply(1, bc_sum) %>% 
  t() %>% 
  as_tibble(rownames = "Temperature") %>% 
  mutate(Temperature = as.integer(Temperature))

p_temp <- ggplot(vc_diag, aes(x = Temperature)) + theme_classic() + 
  geom_line(aes(y = Lower), colour = "grey50", linetype = 2) + 
  geom_line(aes(y = Upper), colour = "grey50", linetype = 2) + 
  geom_line(aes(y = Mean)) + 
  scale_y_log10() + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), 
       y = "Variance (log scale)") + 
  theme(axis.title = element_text(face = "bold"))
