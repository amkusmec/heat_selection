# Plot the genetic variance for different temperature responses on the constant 
# B-spline basis

vc_diag <- apply(vc[-1, -1, ], 3, diag) %>% 
  apply(1, bc_sum) %>% 
  t() %>% 
  as_tibble(rownames = "Temperature") %>% 
  mutate(Temperature = as.integer(Temperature))

p_temp <- ggplot(vc_diag, aes(x = Temperature)) + theme_classic() + 
  geom_step(aes(y = Lower), linetype = 2, colour = "grey50") + 
  geom_step(aes(y = Upper), linetype = 2, colour = "grey50") + 
  geom_step(aes(y = Mean)) + 
  scale_y_log10() + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), 
       y = expression(bold(log[10]*"(Variance)")))
