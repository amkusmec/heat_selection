# Plot the results of the clustering analysis on temperature response trajectories

cohort_post <- read_csv("data/regression_results2/cohort_clusters.csv") %>% 
  mutate(value = case_when(value == 3 ~ 3, 
                           value == 2 ~ 1, 
                           value == 1 ~ 2))

cohort_post %>% 
  rename(Year = Cohort, Mean_Effect = Mean, Cluster = value) %>% 
  write_csv("data/supplementary_tables/ST06.clustered_temperature_means.csv")

sig95 <- sel_post %>% 
  filter(Alpha == "0.95") %>% 
  filter(sign(Lower) == sign(Upper)) %>% 
  pull(Temperature)
sig90 <- sel_post %>% 
  filter(Alpha == "0.9") %>% 
  filter(sign(Lower) == sign(Upper)) %>% 
  pull(Temperature) %>% 
  setdiff(sig95)
sig_na <- setdiff(sel_post$Temperature, c(sig95, sig90))
sig <- bind_rows(tibble(Temperature = sig95, LT = "0.95"), 
                 tibble(Temperature = sig90, LT = "0.90"), 
                 tibble(Temperature = sig_na, LT = "NA"))

cohort_post <- inner_join(cohort_post, sig, b = "Temperature")

cohort_sc <- colorRampPalette(c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", 
                                "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", 
                                "#800026"))
cluster_lbl <- c(`1` = "bold('30-35'*degree*'C')", 
                 `2` = "bold('36'*degree*'C')", 
                 `3` = "bold('37+'*degree*'C')")

p_clusters <- ggplot(cohort_post) + theme_classic() + 
  geom_smooth(aes(x = Cohort, y = Mean, colour = Temperature, group = Temperature, linetype = LT),  
              formula = y ~ splines::bs(x, 10), method = "lm", se = FALSE, size = 0.75) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "black") + 
  scale_colour_stepsn(colours = cohort_sc(length(30:41)), 
                      n.breaks = length(30:41), show.limits = TRUE) +  
  scale_linetype_manual(values = c("0.95" = 1, "0.90" = 2, "NA" = 3)) +
  scale_x_continuous(breaks = seq(1920, 2020, 20), labels = seq(1920, 2020, 20)) + 
  facet_wrap(~ value, ncol = 1, strip.position = "right", 
             labeller = as_labeller(cluster_lbl, default = label_parsed)) + 
  labs(x = "", y = "Scaled coefficient", colour = expression(bold(degree*"C")), 
       linetype = expression(bold(alpha))) + 
  # guides(linetype = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title = element_text(face = "bold"), 
        panel.background = element_rect(fill = "grey80"), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        strip.text = element_text(face = "bold"),
        legend.position = "right", 
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold"), 
        legend.key.height = unit(0.1, "npc"))
