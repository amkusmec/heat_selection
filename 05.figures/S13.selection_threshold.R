library(tidyverse)
source("src/bc_sum.R")


basis <- c("CUB", "STEP")
sel_full <- read_csv("data/regression_results2/selection_full_posterior.csv")

sel_sig <- map_df(seq(0.95, 0.75, -0.05), function(a) {
      sel_full %>% 
        group_by(Basis, Temperature) %>% 
        summarise(BC = bc_sum(Slope, alpha = a) %>% enframe(), 
                  .groups = "drop") %>% 
        unnest(c(BC)) %>% 
        pivot_wider(names_from = "name", values_from = "value") %>% 
        filter(sign(Lower) == sign(Upper)) %>% 
        mutate(Alpha = a)
    }) %>% 
  mutate(Temperature = if_else(Temperature == "Intercept", -2L, as.integer(Temperature)))

ggplot(sel_sig) + theme_classic() + 
  geom_tile(aes(x = Temperature, y = Alpha, fill = I(as.character(sign(Mean))))) + 
  scale_x_continuous(breaks = c(-2, 0:41), labels = c("Int.", as.character(0:41))) + 
  scale_y_continuous(breaks = seq(0.75, 0.95, 0.05), 
                     labels = seq(0.75, 0.95, 0.05)) + 
  scale_fill_manual(values = c("1" = scales::muted("blue"), 
                               "-1" = scales::muted("red")), 
                    labels = c("+", "-")) + 
  facet_wrap(~ Basis, ncol = 1, strip.position = "right", 
             labeller = as_labeller(c("CUB" = "CUBIC", "STEP" = "CONSTANT"))) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), y = expression(bold(alpha)), 
       fill = "Direction") + 
  theme(axis.title = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"))
ggsave("figures/SF13_selection_thresholds.png", 
       width = 9, height = 3.5, units = "in")
