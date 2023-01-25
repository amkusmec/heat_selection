library(tidyverse)
# library(fda)
library(splines)


temperature <- read_csv("data/strict/temperature_noI.csv") %>% 
  dplyr::select(-Fixed) %>% 
  rename(Value = Variable) %>% 
  pivot_wider(names_from = "Temperature", values_from = "Value")

TEMP_eval <- names(temperature)[-(1:3)] %>% as.numeric()
HOURS <- simplify2array(temperature[, -(1:3)]) %>% t()

### Minimal cubic B-spline basis without an intercept has 3 basis functions
##   nknots = df - degree + 2
##      2   =  3 -    3   + 2 (2 boundary knots; no internal knots)
##  Maximal basis has a number of functions equal to evaluation points (43).
### In practice, 43 basis functions will provide a perfect fit. Practically, it 
### isn't useful to go above about 43/2 ~= 21 basis functions (or 18 internal
### knots) anyway.

nk_vec <- 2:20
mse_mat <- sapply(nk_vec, function(nk) {
  kn <- quantile(TEMP_eval, seq(0, 1, length.out = nk))
  H <- bs(TEMP_eval, knots = kn[setdiff(1:nk, c(1, nk))],
          degree = 3, intercept = FALSE)

  a <- tcrossprod(solve(crossprod(H)), H) %*% HOURS
  HOURS_p <- H %*% a
  
  # MSE of individual exposure functions
  sqrt(colMeans((HOURS - HOURS_p)^2))
})


# Relative reduction in MSE, similar to Basna et al. (2022), eq. 9
# Use of various thresholds to assess sensitivity (0.11 is the lowest threshold 
# at which all sample curves reach the stopping criterion)
nk_indiv <- sapply(c(0.01, 0.05, 0.1), function(thresh) {
    apply(mse_mat, 1, function(x) {
        temp <- abs(diff(x))/abs(x[-1])
        nk_vec[min(which(temp <= thresh))]
      })
  })
colnames(nk_indiv) <- c(0.01, 0.05, 0.1)

### Count the number of trials that selected each knot number as their optimum 
### nested within threshold. This also performs some padding for plotting and 
### converts to percentages for scaling.
nk_tbl <- as_tibble(nk_indiv) %>% 
  pivot_longer(everything(), names_to = "Threshold", values_to = "Nk") %>% 
  count(Threshold, Nk) %>% 
  mutate(Nk = if_else(is.na(Nk), 0L, Nk)) %>% 
  group_by(Threshold) %>% 
  group_modify(~ {
      ms <- setdiff(c(0, nk_vec), .x$Nk)
      if (length(ms) != 0) {
        .x <- bind_rows(.x, tibble(Nk = ms, n = 0))
      }
      
      .x %>% mutate(n = n/sum(n))
    }) %>% 
  ungroup()

### Identify the mode of the selected knot numbers. Padding is for plotting 
### purposes.
nk_max <- nk_tbl %>% 
  group_by(Threshold) %>% 
  filter(Nk != 0) %>% 
  filter(n == max(n)) %>% 
  ungroup() %>% 
  mutate(LL = "*") %>% 
  group_by(Nk) %>% 
  group_modify(~ {
      ms <- setdiff(colnames(nk_indiv), .x$Threshold)
      if (length(ms) != 0) {
        .x <- bind_rows(.x, tibble(Threshold = ms, n = 0.1, LL = " "))
      }
      
      .x
    }) %>% 
  ungroup()

ggplot(nk_tbl) + theme_classic() + 
  geom_col(aes(x = Nk, y = n, group = Threshold, fill = Threshold), 
           alpha = 0.6, colour = "black", position = position_dodge(0.9)) + 
  geom_text(aes(x = Nk, y = I(n + 0.005), label = LL, group = Threshold), 
            nk_max, position = position_dodge(0.9), size = 5) + 
  scale_x_continuous(breaks = c(0, nk_vec), labels = c("NA", as.character(nk_vec))) + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) + 
  scale_fill_viridis_d() + 
  labs(x = "# of knots", y = "% trials", 
       fill = expression(bold(theta))) + 
  theme(axis.title = element_text(face = "bold"))
ggsave("figures/exposure_basis_mse.png", 
       width = 7, height = 3, units = "in")

### The optimum is 5 knots (2 boundary, 3 internal) for 0.05 and 0.1. The 0.01 
### threshold has a tie for 5 and 7 knots, leading to a median of 6 (2 boundary, 
### and 4 internal). Due to the large proportion of non-converging trials (~40%), 
### it seems better to use 5 knots.
