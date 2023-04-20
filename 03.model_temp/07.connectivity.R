library(tidyverse)
library(Matrix)
library(abind)
library(grid)
library(gridExtra)
source("src/haversine.R")

fill_sc <- c("County" = "#8dd3c7", "Year" = "#ffffb3", 
             "Trial" = "#bebada", "Hybrid" = "#fb8072")

reg_dat <- read_rds("data/strict/regression_data.rds")

yr_mat <- sparse.model.matrix(~ 0 + as.character(Year), data = reg_dat, 
                              drop.unused.levels = TRUE)
ct_mat <- sparse.model.matrix(~ 0 + County, data = reg_dat, 
                              drop.unused.levels = TRUE)
hy_mat <- sparse.model.matrix(~ 0 + Variety, data = reg_dat, 
                              drop.unused.levels = TRUE)
yc_mat <- sparse.model.matrix(~ 0 + as.character(Year):County, data = reg_dat, 
                              drop.unused.levels = TRUE)
yc_mat <- yc_mat[, colSums(yc_mat) > 0]

yr_poly <- poly(1934:2014, degree = 2, simple = TRUE)

yr_A <- paste0("data/bootstraps2/CUB_vc_raw.csv") %>% 
    read_csv() %>% 
    filter(grp == "State") %>% 
    dplyr::select(-grp) %>% 
    split(.$Bootstrap) %>% 
    map(function(df) {
      yr_poly %*% 
        matrix(c(df$vcov[1], df$vcov[3], 
                 df$vcov[3], df$vcov[2]), 
               nrow = 2, byrow = TRUE) %*% 
        t(yr_poly)
    }) %>% 
  abind(along = 3) %>% 
  apply(1:2, mean)
rownames(yr_A) <- colnames(yr_A) <- as.character(1934:2014)

temp_cty <- read_csv("data/strict/trials_noI.csv", col_types = cols()) %>% 
  distinct(County, .keep_all = TRUE) %>% 
  arrange(County)
cty_A <- matrix(0, nrow = nrow(temp_cty), ncol = nrow(temp_cty))
for (i in 1:nrow(temp_cty)) {
  for (j in 1:nrow(temp_cty)) {
    cty_A[i, j] <- haversine(temp_cty$Longitude[i], temp_cty$Longitude[j], 
                             temp_cty$Latitude[i], temp_cty$Latitude[j])
  }
}
cty_A <- exp(-0.001*cty_A)
rownames(cty_A) <- colnames(cty_A) <- temp_cty$County

hy_A <- Diagonal(n = ncol(hy_mat), x = 2)

yc_A <- kronecker(yr_A, cty_A, make.dimnames = TRUE)
yc_names <- colnames(yc_mat) %>% 
  str_remove("as\\.character\\(Year\\)") %>% 
  str_remove("County")
yc_A <- yc_A[yc_names, yc_names]

connectivity <- function(X, Y, N, A = NULL) {
  if (is.null(A)) {
    adj <- crossprod(X, Y) %*% Diagonal(ncol(Y)) %*% crossprod(Y, X)
  } else {
    adj <- crossprod(X, Y) %*% A %*% crossprod(Y, X)
  }
  
  adj <- adj/outer(N, N)
  unlist(
    sapply(1:(nrow(adj) - 1), function(i) {
      cat(i, "\n")
      diag(adj)[i] + diag(adj)[(i + 1):nrow(adj)] - 2*adj[(i + 1):nrow(adj), i]
    })
  )
}

con_ct_hy <- connectivity(ct_mat, hy_mat, reg_dat %>% count(County) %>% pull(n), hy_A)
con_hy_ct <- connectivity(hy_mat, ct_mat, reg_dat %>% count(Variety) %>% pull(n), cty_A)

con_yr_hy <- connectivity(yr_mat, hy_mat, reg_dat %>% count(Year) %>% pull(n), hy_A)
con_hy_yr <- connectivity(hy_mat, yr_mat, reg_dat %>% count(Variety) %>% pull(n), yr_A)

con_ct_yr <- connectivity(ct_mat, yr_mat, reg_dat %>% count(County) %>% pull(n), yr_A)
con_yr_ct <- connectivity(yr_mat, ct_mat, reg_dat %>% count(Year) %>% pull(n), cty_A)

con_tr_hy <- connectivity(yc_mat, hy_mat, reg_dat %>% count(County, Year) %>% pull(n), hy_A)
con_hy_tr <- connectivity(hy_mat, yc_mat, reg_dat %>% count(Variety) %>% pull(n), yc_A)



df_ch <- bind_rows(enframe(con_ct_hy) %>% mutate(name = "County"), 
                   enframe(con_hy_ct) %>% mutate(name = "Hybrid")) %>% 
  mutate(binned = floor(value/0.05)*0.05) %>% 
  count(name, binned) %>% 
  group_by(name) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = if_else(name == "County", -1*prop, prop))

p1 <- ggplot(df_ch, aes(x = binned, y = prop)) + theme_classic() + 
  geom_col(aes(fill = name), colour = "black", alpha = 0.8) + 
  scale_fill_manual(values = fill_sc) + 
  scale_x_continuous(limits = c(-0.1, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(-1, 1), 
                     breaks = c(seq(-1, -0.25, 0.25), 0, seq(0.25, 1, 0.25)), 
                     labels = ~ scales::label_percent()(abs(.x))) + 
  labs(y = expression(bold("[Counties] "*symbol('\254')*" % contrasts "*symbol('\256')*" [Hybrids]")), 
       x = "Contrast variance", tag = "A") + 
  theme(panel.grid.major.x = element_line(colour = "grey80"), 
        axis.title = element_text(face = "bold")) + 
  guides(fill = "none") + 
  coord_flip()


df_yh <- bind_rows(enframe(con_yr_hy) %>% mutate(name = "Year"), 
                   enframe(con_hy_yr) %>% mutate(name = "Hybrid")) %>% 
  mutate(binned = floor(value/0.05)*0.05) %>% 
  count(name, binned) %>% 
  group_by(name) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = if_else(name == "Year", -1*prop, prop))

p2 <- ggplot(df_yh, aes(x = binned, y = prop)) + theme_classic() + 
  geom_col(aes(fill = name), colour = "black", alpha = 0.8) + 
  scale_fill_manual(values = fill_sc) + 
  scale_x_continuous(limits = c(-0.1, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(-1, 1), 
                     breaks = c(seq(-1, -0.25, 0.25), 0, seq(0.25, 1, 0.25)), 
                     labels = ~ scales::label_percent()(abs(.x))) + 
  labs(y = expression(bold("[Years] "*symbol('\254')*" % contrasts "*symbol('\256')*" [Hybrids]")), 
       x = "Contrast variance", tag = "B") + 
  theme(panel.grid.major.x = element_line(colour = "grey80"), 
        axis.title = element_text(face = "bold")) + 
  guides(fill = "none") + 
  coord_flip()


df_cy <- bind_rows(enframe(con_ct_yr) %>% mutate(name = "County"), 
                   enframe(con_yr_ct) %>% mutate(name = "Year")) %>% 
  mutate(binned = floor(value/0.05)*0.05) %>% 
  count(name, binned) %>% 
  group_by(name) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = if_else(name == "County", -1*prop, prop))

p3 <- ggplot(df_cy, aes(x = binned, y = prop)) + theme_classic() + 
  geom_col(aes(fill = name), colour = "black", alpha = 0.8) + 
  scale_fill_manual(values = fill_sc) + 
  scale_x_continuous(limits = c(-0.1, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(-1, 1), 
                     breaks = c(seq(-1, -0.25, 0.25), 0, seq(0.25, 1, 0.25)), 
                     labels = ~ scales::label_percent()(abs(.x))) + 
  labs(y = expression(bold("[Counties] "*symbol('\254')*" % contrasts "*symbol('\256')*" [Years]")), 
       x = "Contrast variance", tag = "C") + 
  theme(panel.grid.major.x = element_line(colour = "grey80"), 
        axis.title = element_text(face = "bold")) + 
  guides(fill = "none") + 
  coord_flip()


df_th <- bind_rows(enframe(con_tr_hy) %>% mutate(name = "Trial"), 
                   enframe(con_hy_tr) %>% mutate(name = "Hybrid")) %>% 
  mutate(binned = floor(value/0.1)*0.1) %>% 
  count(name, binned) %>% 
  group_by(name) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = if_else(name == "Trial", -1*prop, prop))

p4 <- ggplot(df_th, aes(x = binned, y = prop)) + theme_classic() + 
  geom_col(aes(fill = name), colour = "black", alpha = 0.8) + 
  scale_fill_manual(values = fill_sc) + 
  # scale_x_continuous(limits = c(-0.1, 1), labels = scales::label_percent()) + 
  scale_y_continuous(limits = c(-1, 1), 
                     breaks = c(seq(-1, -0.25, 0.25), 0, seq(0.25, 1, 0.25)), 
                     labels = ~ scales::label_percent()(abs(.x))) + 
  labs(y = expression(bold("[Trials] "*symbol('\254')*" % contrasts "*symbol('\256')*" [Hybrids]")), 
       x = "Contrast variance", tag = "D") + 
  theme(panel.grid.major.x = element_line(colour = "grey80"), 
        axis.title = element_text(face = "bold")) + 
  guides(fill = "none") + 
  coord_flip()


gp <- arrangeGrob(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), ggplotGrob(p4), 
                  layout_matrix = matrix(1:4, ncol = 2, byrow = TRUE))
ggsave("figures/SF12_contrast_connectivity.png", gp, 
       width = 10, height = 6, units = "in")
