

cov_int <- read_rds("data/regression_results2/cohort_cov_all.rds") %>% 
  `[[`(2) %>% 
  `[`(-(1:3), , )

years <- cohort_post %>% pull(Cohort) %>% unique() %>% sort()

cov_post <- map_df(c(0.95, 0.9), function(a) {
      apply(cov_int, 1:2, bc_sum, alpha = a) %>% 
        apply(3, function(x) {
            as_tibble(t(x)) %>% 
              mutate(Temperature = 2:41)
          }, simplify = FALSE) %>% 
        bind_rows() %>% 
        mutate(Cohort = rep(years, each = length(2:41)), 
               Sig = if_else(sign(Lower) == sign(Upper), a, 0))
    }) %>% 
  mutate(Sig = recode_factor(Sig, "0.95" = "*", "0.9" = "^", "0" = "")) %>% 
  mutate(Bin = floor((Temperature + 1)/3), 
         Bin = if_else(Bin == 14, 13, Bin)) %>% 
  group_by(Cohort, Bin) %>% 
  mutate(Label = paste0(min(Temperature), "-", max(Temperature))) %>% 
  slice(2L) %>% 
  ungroup() %>% 
  group_by(Cohort, Temperature) %>% 
  arrange(Sig) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  mutate(Label = factor(Label, levels = unique(Label), ordered = TRUE))

p_cov <- ggplot(cov_post) + theme_classic() + 
  geom_tile(aes(x = Cohort, y = Label, fill = Mean)) + 
  geom_text(aes(x = Cohort, y = Label, label = Sig)) + 
  scale_fill_steps2(limits = c(-1, 1), show.limits = TRUE, n.breaks = 10) + 
  scale_x_continuous(breaks = seq(1940, 2010, 10), 
                     labels = seq(1940, 2010, 10)) + 
  labs(x = "", y = expression(bold("Temperature ("*degree*"C)")), 
       fill = expression(bold("cor("*italic(l[i])*","*italic(beta[ih])*")"))) + 
  theme(axis.title = element_text(face = "bold"))
