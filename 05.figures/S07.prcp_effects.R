library(tidyverse)
source("src/bc_sum.R")

basis <- c("CUB", "STEP")

prcp_sc <- read_rds("data/regression_data/reg_dat_CUB.rds")
prcp_attr <- attributes(prcp_sc$PRCP)
prcp_sc <- prcp_sc %>% 
  pull(PRCP) %>% 
  range()
prcp_sc <- seq(prcp_sc[1], prcp_sc[2], length.out = 100L)
prcp_o <- prcp_attr$`scaled:scale`*prcp_sc + prcp_attr$`scaled:center`

pp <- map_df(basis, function(b) {
  paste0("data/regression_results2/", b, "_results/", 
         b, "_precipitation_fixed.rds") %>% 
    read_rds() %>% 
    apply(1, bc_sum) %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(Basis = b, 
           X = prcp_o) %>% 
    mutate(across(Lower:Upper, ~ if_else(is.nan(.x), 0, .x))) %>% 
    select(Basis:X, everything())
})

ggplot(pp, aes(x = X)) + theme_classic() + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.5, fill = "grey80") + 
  geom_line(aes(y = Mean)) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red") + 
  facet_wrap(~ Basis, labeller = as_labeller(c("CUB" = "Cubic", "STEP" = "Constant"))) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = seq(200, 800, 200), 
                     labels = seq(200, 800, 200), 
                     limits = c(225, NA)) + 
  labs(x = "Precipitation (mm)", y = "% yield/mm") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"))
ggsave("figures/revision1/prcp_functions.png", width = 6, height = 2.5, units = "in")
