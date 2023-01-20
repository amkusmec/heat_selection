
vc <- read_rds("data/regression_results2/CUB_results/CUB_temperature_vc.rds") %>% 
  `[`(-2, -2, )


vc_int <- enframe(vc[1, 1, ], name = "Bootstrap", value = "vcov") %>% 
  mutate(grp = "Variety") %>% 
  select(Bootstrap, grp, vcov)
vc_temp <- map_df(1:(dim(vc)[3]), function(i) {
    tibble_row(Bootstrap = i, 
               grp = "Temperature", 
               vcov = sum(vc[-1, -1, i]))
  })
vc_other <- read_csv("data/bootstraps2/CUB_vc_raw.csv") %>% 
  filter(grp != "Variety") %>% 
  filter((var1 == var2) | if_all(var1:var2, ~ is.na(.x))) %>% 
  group_by(Bootstrap, grp) %>% 
  summarise(vcov = sum(vcov), 
            .groups = "drop") %>% 
  mutate(grp = if_else(grp == "State", "Time", grp))

vc_all <- bind_rows(vc_other, vc_int, vc_temp) %>% 
  group_by(Bootstrap) %>% 
  mutate(Pct = vcov/sum(vcov)) %>% 
  ungroup() %>% 
  group_by(grp) %>% 
  summarise(B = enframe(bc_sum(Pct)), 
            .groups = "drop") %>% 
  unnest(c(B)) %>% 
  pivot_wider(names_from = "name", values_from = "value") %>% 
  mutate(grp = factor(grp, levels = c("County", "Time", "Variety", 
                                      "Temperature", "Residual"), 
                      ordered = TRUE))

p_all <- ggplot(vc_all) + theme_classic() + 
  geom_linerange(aes(x = grp, ymin = Lower, ymax = Upper)) +
  geom_point(aes(x = grp, y = Mean)) +
  # geom_violin(aes(x = grp, y = Pct)) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(x = "", y = "% variance") + 
  theme(axis.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1))
