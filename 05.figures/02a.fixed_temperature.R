# Plot the fixed effects coefficient function for temperature responses

fixf <- read_rds("data/regression_results2/CUB_results/CUB_temperature_fixed.rds") %>% 
  `[`(-1, )
fixc <- map_df(c(0.95, 0.9), function(a) {
      apply(fixf, 1, bc_sum, alpha = a) %>% 
        t() %>% 
        as_tibble() %>% 
        mutate(Alpha = a, 
               Temperature = 0:41) %>% 
        pivot_longer(Mean:Upper, names_to = "Measure", values_to = "Value")
    }) %>% 
  mutate(Alpha = factor(Alpha))


p_fixed <- ggplot() + theme_classic() + 
  geom_line(aes(x = Temperature, y = Value), filter(fixc, Measure == "Mean")) + 
  geom_line(aes(x = Temperature, y = Value, linetype = Alpha), 
            filter(fixc, Measure == "Lower"), colour = "grey50") + 
  geom_line(aes(x = Temperature, y = Value, linetype = Alpha), 
            filter(fixc, Measure == "Upper"), colour = "grey50") + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red") + 
  scale_y_continuous(labels = scales::label_percent()) + 
  scale_linetype_manual(values = c("0.95" = 2, "0.9" = 3)) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), 
       y = "% yield/h exposure", 
       linetype = expression(bold(alpha))) + 
  theme(axis.title = element_text(face = "bold"), 
        legend.position = c(0.25, 0.25))
