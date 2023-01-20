library(tidyverse)
library(abind)
source("src/bc_sum.R")


cohort_width <- 1
hyb_counts <- read_rds("data/regression_data/reg_dat_CUB.rds") %>% 
  mutate(Year = attr(Year, "scaled:scale")*Year + attr(Year, "scaled:center")) %>% 
  group_by(Variety) %>% 
  mutate(Year = cohort_width*floor(min(Year)/cohort_width), 
         .groups = "drop") %>% 
  ungroup() %>% 
  count(Variety, Year) %>% 
  group_by(Year) %>% 
  mutate(Wt = n/sum(n)) %>% 
  ungroup()
gr_lvls <- unique(hyb_counts$Year) %>% sort()

# basis <- c("CUB", "LIN", "STEP")
basis <- c("CUB", "STEP")

cohort_means <- map(basis, function(b) {
  paste0("data/regression_results2/", b, "_results/", 
         b, "_temperature_random.rds") %>% 
    read_rds() %>% 
    apply(c(1, 3), function(x) {
      sapply(gr_lvls, function(g) {
        weighted.mean(x[hyb_counts$Year == g], hyb_counts$Wt[hyb_counts$Year == g], 
                      na.rm = TRUE)
      })
    })
})

names(cohort_means) <- basis
for (i in seq_along(cohort_means)) dimnames(cohort_means[[i]]) <- list(gr_lvls, c("Intercept", -1:41), NULL)
write_rds(cohort_means, "data/regression_results2/cohort_means_all.rds")

cohort_post <- map2_df(cohort_means, names(cohort_means), function(m, n) {
      temp <- apply(m, 1:2, bc_sum)
      map(1:(dim(temp)[1]), function(i) {
            temp[i, , ] %>% 
              as_tibble(rownames = "Cohort") %>% 
              pivot_longer(-Cohort, names_to = "Temperature", 
                           values_to = dimnames(temp)[[1]][i])
          }) %>% 
        reduce(inner_join, by = c("Cohort", "Temperature")) %>% 
        mutate(Basis = n) %>% 
        select(Basis, everything())
    }) %>% 
  mutate(Cohort = as.integer(Cohort))

cohort_cov <- map(basis, function(b) {
  temp <- paste0("data/regression_results2/", b, "_results/", 
         b, "_temperature_random.rds") %>% 
    read_rds()
  
  apply(temp, 3, function(m) {
        # Returns temperatures as rows and years as columns
        sapply(gr_lvls, function(g) {
          idx <- which(hyb_counts$Year == g)
          if (all(is.na(m[, idx]))) {
            rep(as.numeric(NA), nrow(m) - 1)
          } else {
            cor(m[1, idx], t(m[-1, idx]), use = "complete") %>% 
              drop()
          }
        })
      }, simplify = FALSE) %>% 
    abind(along = 3)
})
write_rds(cohort_cov, "data/regression_results2/cohort_cov_all.rds")

cohort_cov_post <- map2_df(basis, cohort_cov, function(b, a) {
      apply(a, 1:2, bc_sum) %>% 
        apply(3, function(x) {
            as_tibble(t(x)) %>% 
              mutate(Temperature = -1:41)
          }, simplify = FALSE) %>% 
        bind_rows() %>% 
        mutate(Basis = b, 
               Cohort = rep(gr_lvls, each = 43)) %>% 
        select(Basis:Cohort, Temperature, everything())
    }) %>% 
  filter(Temperature >= 0) %>% 
  filter(!(Basis == "STEP" & Temperature < 2))

ggplot(filter(cohort_post, Temperature == "41")) + theme_bw() + 
  geom_ribbon(aes(x = Cohort, ymin = Lower, ymax = Upper), fill = "grey50", alpha = 0.5) + 
  geom_line(aes(x = Cohort, y = Mean)) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  facet_wrap(Basis ~ .)

ggplot(filter(cohort_cov_post, Temperature == "35")) + theme_bw() + 
  geom_ribbon(aes(x = Cohort, ymin = Lower, ymax = Upper), fill = "grey50", alpha = 0.5) + 
  geom_line(aes(x = Cohort, y = Mean)) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  facet_wrap(Basis ~ .)

ggplot(cohort_cov_post) + theme_classic() + 
  geom_tile(aes(x = Cohort, y = Temperature, fill = Mean)) + 
  scale_fill_steps2(limits = c(-1, 1), show.limits = TRUE, n.breaks = 10) + 
  scale_x_continuous(breaks = seq(1940, 2010, 10), 
                     labels = seq(1940, 2010, 10)) + 
  facet_wrap(~ Basis, ncol = 1, strip.position = "right") + 
  labs(x = "", y = expression("Temperature ("*degree*"C)"), 
       fill = expression("cor("*l[i]*","*beta[ih]*")")) + 
  theme(strip.text = element_text(face = "bold"))
ggsave("figures/int_temp_cor.png", width = 8, height = 4, units = "in")

# bind_rows(mutate(cohort_post, Variable = "Mean", Temperature = as.integer(Temperature)), 
#           mutate(cohort_cov_post, Variable = "Covariance")) %>% 
#   filter(Temperature == "35") %>% 
#   ggplot(aes(x = Cohort)) + theme_bw() + 
#     geom_line(aes(y = Mean)) + 
#     geom_hline(yintercept = 0, linetype = 2) + 
#     facet_grid(Variable ~ Basis, scales = "free_y")
# 
# bind_rows(mutate(cohort_post, Variable = "Mean", Temperature = as.integer(Temperature)), 
#           mutate(cohort_cov_post, Variable = "Covariance")) %>% 
#   filter(Temperature == 35) %>% 
#   select(-Lower, -Upper) %>% 
#   pivot_wider(names_from = "Variable", values_from = "Mean") %>% 
#   ggplot() + theme_bw() + 
#     geom_path(aes(x = Mean, y = Covariance)) + 
#     geom_point(aes(x = Mean, y = Covariance, colour = Cohort)) + 
#     geom_hline(yintercept = 0, linetype = 2) + 
#     geom_vline(xintercept = 0, linetype = 2) + 
#     scale_colour_viridis_b(n.breaks = 8, show.limits = TRUE) + 
#     facet_wrap(Basis ~ .)
# 
# bind_rows(mutate(cohort_post, Variable = "Mean", Temperature = as.integer(Temperature)), 
#           mutate(cohort_cov_post, Variable = "Covariance")) %>% 
#   filter(!is.na(Temperature)) %>% 
#   filter(!is.na(Lower)) %>% 
#   select(-Lower, -Upper) %>% 
#   pivot_wider(names_from = "Variable", values_from = "Mean") %>% 
#   group_by(Basis, Temperature) %>% 
#   summarise(R = cor(Mean, Covariance, use = "complete"), 
#             .groups = "drop")

cmat <- cohort_post %>% 
  mutate(Temperature = as.integer(Temperature)) %>% 
  filter(Basis == "CUB", Temperature >= 30) %>% 
  select(Cohort:Mean) %>% 
  pivot_wider(names_from = "Temperature", values_from = "Mean") %>% 
  select(-Cohort) %>% 
  mutate(across(everything(), ~ scale(.x) %>% drop())) %>% 
  as.matrix() %>% 
  t()
ss <- lapply(2:6, function(i) kmeans(cmat, centers = i))
sapply(ss, function(k) k$betweenss/k$totss)

cohort_post_sc <- cohort_post %>% 
  mutate(Temperature = as.integer(Temperature)) %>% 
  filter(Basis == "CUB", Temperature >= 30) %>% 
  select(Cohort:Mean) %>% 
  group_by(Temperature) %>% 
  mutate(Mean = scale(Mean) %>% drop()) %>% 
  ungroup() %>%  
  inner_join(enframe(ss[[2]]$cluster) %>% 
               mutate(name = as.integer(name)), 
             by = c("Temperature" = "name"))
write_csv(cohort_post_sc, "data/regression_results2/cohort_clusters.csv")

cohort_sc <- colorRampPalette(c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", 
                                "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", 
                                "#800026"))
cluster_lbl <- c(`3` = "30-35~degree~C", 
                 `2` = "36~degree~C", 
                 `1` = "37+~degree~C")

ggplot(cohort_post_sc) + theme_bw() + 
  # annotate("rect", xmin = 1933, xmax = 1943, ymin = -Inf, ymax = Inf, 
  #          fill = "red", alpha = 0.3) + 
  # annotate("rect", xmin = 2005, xmax = 2015, ymin = -Inf, ymax = Inf, 
  #          fill = "blue", alpha = 0.3) + 
  geom_smooth(aes(x = Cohort, y = Mean, colour = Temperature, group = Temperature),  
              formula = y ~ splines::bs(x, 8), method = "lm", se = FALSE, size = 0.75) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "black") + 
  scale_colour_stepsn(colours = cohort_sc(length(30:41)), 
                      n.breaks = length(30:41), show.limits = TRUE) +  
  # scale_linetype_manual(values = c("TRUE" = 1, "FALSE" = 2)) + 
  scale_x_continuous(breaks = seq(1920, 2020, 20), labels = seq(1920, 2020, 20)) + 
  facet_wrap(~ value, ncol = 1, strip.position = "right", 
             labeller = as_labeller(cluster_lbl, default = label_parsed)) + 
  labs(x = "", y = "Scaled coefficient", colour = expression(degree*"C")) + 
  # guides(linetype = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_rect(fill = "grey80"), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        strip.text = element_text(face = "bold"),
        legend.position = "right", 
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold", size = 14), 
        legend.key.height = unit(0.15, "npc"))
ggsave("figures/cohort_clusters.png", width = 6, height = 4.5, units = "in")

cohort_post %>% 
  mutate(Temperature = as.integer(Temperature)) %>% 
  filter(Temperature >= 30) %>% 
  ggplot() + theme_bw() + 
    geom_smooth(aes(x = Cohort, y = Mean, colour = Temperature, linetype = Basis), 
                formula = y ~ splines::bs(x, 8), method = "lm", se = FALSE, size = 0.75) + 
    geom_hline(yintercept = 0, linetype = 2) + 
    facet_wrap(~ Temperature, scales = "free_y")

cohort_post %>% 
  mutate(Temperature = as.integer(Temperature)) %>% 
  filter(Temperature >= 30) %>% 
  group_by(Basis, Temperature) %>% 
  mutate(Mean = scale(Mean) %>% drop()) %>% 
  ungroup() %>% 
  ggplot() + theme_bw() + 
    geom_smooth(aes(x = Cohort, y = Mean, colour = Temperature, group = Temperature), 
                formula = y ~ splines::bs(x, 8), method = "lm", se = FALSE, size = 0.75) + 
    geom_hline(yintercept = 0, linetype = 2) + 
    facet_wrap(~ Basis, ncol = 1, scales = "free_y")
