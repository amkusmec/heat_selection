# Plot the cohort-wise correlations between hybrid intercept and temperature 
# responses

cov_int <- read_rds("data/regression_results2/cohort_cov_all.rds") %>% 
  `[[`(1) %>% 
  `[`(-1, , )

years <- cohort_post %>% pull(Cohort) %>% unique() %>% sort()

map_df(1:(dim(cov_int)[3]), function(i) {
      as_tibble(cov_int[, , i]) %>% 
        mutate(Temperature = 0:41) %>% 
        pivot_longer(-Temperature, names_to = "Year", values_to = "Correlation") %>% 
        mutate(Year = rep_along(Temperature, years), 
               Bootstrap = i) %>% 
        select(Bootstrap, Temperature, Year, Correlation)
    }) %>% 
  write_csv("data/supplementary_tables/ST05.intercept_temperature_correlations.csv")

cov_post <- map_df(c(0.95, 0.9), function(a) {
      apply(cov_int, 1:2, bc_sum, alpha = a) %>% 
        apply(3, function(x) {
              as_tibble(t(x)) %>% 
                mutate(Temperature = 0:41)
            }, simplify = FALSE) %>% 
          bind_rows() %>% 
        mutate(Cohort = rep(years, each = length(0:41)), 
               Sig = if_else(sign(Lower) == sign(Upper), a, 0))
    }) %>% 
  mutate(Sig = recode_factor(Sig, "0.95" = "*", "0.9" = "^", "0" = "")) %>% 
  group_by(Cohort, Temperature) %>% 
  arrange(Sig) %>% 
  slice(1L) %>% 
  ungroup()

p_cov <- ggplot(cov_post) + theme_classic() + 
  geom_tile(aes(x = Cohort, y = Temperature, fill = Mean)) + 
  geom_text(aes(x = Cohort, y = Temperature, label = Sig)) + 
  scale_fill_steps2(limits = c(-1, 1), show.limits = TRUE, n.breaks = 10) + 
  scale_x_continuous(breaks = seq(1940, 2010, 10), 
                     labels = seq(1940, 2010, 10)) + 
  labs(x = "", y = expression(bold("Temperature ("*degree*"C)")), 
       fill = expression(bold("cor("*italic(l[i])*","*italic(beta[ih])*")"))) + 
  theme(axis.title = element_text(face = "bold"))
