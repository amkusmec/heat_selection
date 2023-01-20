
cor_func <- apply(vc, 3, cov2cor, simplify = FALSE) %>% 
  abind(along = 3) %>% 
  apply(1:2, bc_sum) %>% 
  `[`(1, , ) %>% 
  as_tibble(rownames = "Temperature1") %>% 
  pivot_longer(-Temperature1, names_to = "Temperature2", values_to = "Value") %>% 
  mutate(across(Temperature1:Temperature2, ~ if_else(.x == "Intercept", "-2", .x))) %>% 
  mutate(across(where(is.character), ~ as.integer(.x)))

p_cor <- ggplot(cor_func) + theme_classic() + 
  geom_tile(aes(x = Temperature1, y = Temperature2, fill = Value)) + 
  scale_fill_steps2(n.breaks = 10, show.limits = TRUE) + 
  scale_x_continuous(breaks = c(-2, seq(0, 40, 10)), 
                     labels = c("Int.", as.character(seq(0, 40, 10)))) + 
  scale_y_continuous(breaks = c(-2, seq(0, 40, 10)), 
                     labels = c("Int.", as.character(seq(0, 40, 10)))) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), 
       y = expression(bold("Temperature ("*degree*"C)")), 
       fill = expression(bold(r[G])))
