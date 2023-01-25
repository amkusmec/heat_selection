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
  geom_smooth(aes(x = Cohort, y = Mean, colour = Temperature, group = Temperature),  
              formula = y ~ splines::bs(x, 8), method = "lm", se = FALSE, size = 0.75) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "black") + 
  scale_colour_stepsn(colours = cohort_sc(length(30:41)), 
                      n.breaks = length(30:41), show.limits = TRUE) +  
  scale_x_continuous(breaks = seq(1920, 2020, 20), labels = seq(1920, 2020, 20)) + 
  facet_wrap(~ value, ncol = 1, strip.position = "right", 
             labeller = as_labeller(cluster_lbl, default = label_parsed)) + 
  labs(x = "", y = "Scaled coefficient", colour = expression(degree*"C")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_rect(fill = "grey80"), 
        strip.background = element_rect(fill = "white", colour = "black"), 
        strip.text = element_text(face = "bold"),
        legend.position = "right", 
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold", size = 14), 
        legend.key.height = unit(0.15, "npc"))
ggsave("figures/cohort_clusters.png", width = 6, height = 4.5, units = "in")
