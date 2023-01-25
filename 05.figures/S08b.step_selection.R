# Plot selection results for the constant B-spline basis

sel <- read_csv("data/regression_results2/selection_full_posterior.csv") %>% 
  filter(Basis == "STEP")

sel_int <- sel %>% 
  filter(Temperature == "Intercept") %>% 
  select(-Temperature)
sel_int_post <- map_df(c(0.95, 0.9), function(a) {
      bc_sum(sel_int$Slope, alpha = a) %>% 
        enframe(name = "Measure", value = "Value") %>% 
        mutate(Alpha = a)
    }) %>% 
  # mutate(Alpha = as.character(Alpha)) %>%
  pivot_wider(names_from = "Measure", values_from = "Value")

p_int <- ggplot(sel_int_post) + theme_classic() + 
  geom_linerange(aes(ymin = Lower, ymax = Upper), 
                 filter(sel_int_post, Alpha == 0.95), 
                 x = 1, size = 0.75) + 
  geom_linerange(aes(ymin = Lower, ymax = Upper), 
                 filter(sel_int_post, Alpha == 0.9), 
                 x = 1, size = 1.5) + 
  geom_point(aes(y = Mean), sel_int_post, x = 1, size = 4) + 
  scale_x_continuous(limits = c(0, 2), breaks = 1) + 
  scale_y_continuous(labels = scales::label_percent(), limits = c(-0.001, 0.01)) + 
  labs(y = "% yield/yr", x = "Intercept") + 
  theme(axis.title = element_text(face = "bold"), 
        axis.text.x = element_blank())


sel <- sel %>% 
  mutate(Temperature = as.integer(Temperature)) %>% 
  filter(Temperature >= 2)
sel_post <- map_df(c(0.95, 0.9), function(a) {
      sel %>% 
        group_by(Temperature) %>% 
        summarise(S = enframe(bc_sum(Slope, alpha = a)), 
                  .groups = "drop") %>% 
        unnest(c(S)) %>% 
        pivot_wider(names_from = "name", values_from = "value") %>% 
        mutate(Alpha = a)
    }) %>% 
  mutate(Alpha = factor(Alpha))

p_selection <- ggplot(sel_post, aes(x = Temperature)) + theme_classic() + 
  geom_step(aes(x = Temperature, y = Mean)) + 
  geom_step(aes(x = Temperature, y = Lower, linetype = Alpha), colour = "grey50") + 
  geom_step(aes(x = Temperature, y = Upper, linetype = Alpha), colour = "grey50") + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red") + 
  scale_y_continuous(labels = scales::label_percent()) + 
  scale_linetype_manual(values = c("0.95" = 2, "0.9" = 3)) + 
  guides(linetype = "none") + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), 
       y = "% yield/h exposure/yr", 
       linetype = expression(bold(alpha))) + 
  theme(axis.title = element_text(face = "bold"))
