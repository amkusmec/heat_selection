library(tidyverse)
library(abind)
source("src/bc_sum.R")


basis <- c("CUB", "STEP")
tt <- c("Mean", "Lower", "Upper")


vc <- map_df(basis, function(b) {
  vc <- paste0("data/regression_results2/", b, "_results/", b, "_temperature_vc.rds") %>% 
    read_rds() %>% 
    `[`(-2, -2, ) %>% 
    apply(3, cov2cor, simplify = FALSE) %>% 
    abind(along = 3) %>% 
    apply(1:2, bc_sum)

  map(1:3, function(i) {
        vc[i, , ] %>% 
          as_tibble(rownames = "X") %>% 
          pivot_longer(-X, names_to = "Y", values_to = "V") %>% 
          rename(!! tt[i] := V)
      }) %>% 
    reduce(inner_join, by = c("X", "Y")) %>% 
    mutate(across(Lower:Upper, ~ if_else(is.nan(.x), 1, .x))) %>%
    mutate(Sig = if_else(sign(Lower) == sign(Upper), "*", ""), 
           Basis = b) %>% 
    mutate(across(X:Y, ~ as.integer(if_else(.x == "Intercept", "-2", .x)))) %>% 
    select(Basis, everything())
})

vc <- vc %>% 
  filter(!(Basis == "STEP" & (X %in% 0:1 | Y %in% 0:1)))

knots <- bind_rows(tibble(Basis = "CUB", 
                          Knots = quantile(-1:41, seq(0, 1, length.out = 5)[2:4])), 
                   tibble(Basis = "STEP", 
                          Knots = seq(2, 38, 3))) %>% 
  mutate(Knots = Knots - 0.5)

ggplot(vc, aes(x = X, y = Y)) + theme_classic() + 
  geom_tile(aes(fill = Mean)) + 
  geom_hline(aes(yintercept = Knots), knots, linetype = 2, colour = "green", size = 0.4) + 
  geom_vline(aes(xintercept = Knots), knots, linetype = 2, colour = "green", size = 0.4) + 
  scale_x_continuous(breaks = c(-2, seq(0, 41, 5)), 
                     labels = c("Int.", seq(0, 41, 5))) + 
  scale_y_continuous(breaks = c(-2, seq(0, 41, 5)), 
                     labels = c("Int.", seq(0, 41, 5))) + 
  scale_fill_gradient2(limits = c(-1, 1)) + 
  facet_wrap(~ Basis) + 
  labs(x = expression("Temperature ("*degree*"C)"), 
       y = expression("Temperature ("*degree*"C)"), 
       fill = expression(r[G])) + 
  theme(strip.background = element_blank(), 
        strip.text = element_blank())
ggsave("figures/rg_temperature_all.png", width = 10, height = 2.5, units = "in")
