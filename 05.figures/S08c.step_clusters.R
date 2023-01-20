
cohort <- read_rds("data/regression_results2/cohort_means_all.rds") %>% 
  `[[`(2)
cohort_means <- apply(cohort, 1:2, mean, na.rm = TRUE) %>% 
  `[`( , -(1:4)) %>% 
  as_tibble(rownames = "Cohort") %>% 
  pivot_longer(-Cohort, names_to = "Temperature", values_to = "Mean") %>% 
  mutate(Cohort = as.integer(Cohort), 
         Temperature = as.integer(Temperature), 
         Bin = floor((Temperature + 1)/3), 
         Bin = if_else(Bin == 14, 13, Bin)) %>% 
  filter(Bin >= 10) %>% 
  group_by(Cohort, Bin) %>% 
  mutate(Label = paste0("bold('", min(Temperature), "-", max(Temperature), "'*degree*'C')")) %>% 
  slice(2L) %>% 
  ungroup() %>% 
  group_by(Bin) %>% 
  mutate(Mean = scale(Mean) %>% drop()) %>% 
  ungroup()

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
                 tibble(Temperature = sig_na, LT = "NA")) %>% 
  filter(Temperature %in% cohort_means$Temperature)

cohort_post <- inner_join(cohort_means, sig, by = "Temperature")

cohort_sc <- colorRampPalette(c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", 
                                "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", 
                                "#800026"))

p_clusters <- ggplot(cohort_post) + theme_classic() + 
  geom_smooth(aes(x = Cohort, y = Mean, colour = Temperature, group = Temperature, linetype = LT),  
              formula = y ~ splines::bs(x, 10), method = "lm", se = FALSE, size = 1) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "black") + 
  scale_colour_stepsn(colours = cohort_sc(5), breaks = seq(29, 41, 3), show.limits = TRUE) +  
  scale_linetype_manual(values = c("0.95" = 1, "0.90" = 2, "NA" = 3)) +
  scale_x_continuous(breaks = seq(1920, 2020, 20), labels = seq(1920, 2020, 20)) + 
  labs(x = "", y = "Scaled coefficient", colour = expression(bold(degree*"C")), 
       linetype = expression(bold(alpha))) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title = element_text(face = "bold"), 
        panel.background = element_rect(fill = "grey80"), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        strip.text = element_text(face = "bold"),
        legend.position = "right", 
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold"), 
        legend.key.height = unit(0.1, "npc"))
