
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
       y = expression(bold(log[10]*"(Variance)")))
