library(tidyverse)
library(abind)
library(prinsimp)
source("src/bc_sum.R")


basis <- c("CUB", "STEP")
tt <- c("Mean", "Lower", "Upper")
TEMP_eval2 <- -1:41

sim <- map(basis, function(b) {
  vc <- paste0("data/regression_results2/", b, "_results/", b, "_temperature_vc.rds") %>% 
    read_rds()
  
  if (b == "STEP") {
    vc <- vc[-(1:4), -(1:4), ]
    TEMP_eval <- TEMP_eval2[-(1:3)]
  } else {
    vc <- vc[-(1:2), -(1:2), ]
    TEMP_eval <- TEMP_eval2[-1]
  }
  
  
  vc_mean <- apply(vc, 1:2, mean)
  temp_sim_est <- simpart(vc_mean, simpledim = ncol(vc_mean), 
                          x = TEMP_eval, cov = TRUE)
  n_basis <- min(which(cumsum(temp_sim_est$varperc$simple) >= 95))
  
  temp_sim <- lapply(1:(dim(vc)[3]), function(i) {
    temp <- simpart(vc[, , i], simpledim = ncol(vc), 
                    x = TEMP_eval, cov = TRUE)
    list(varperc = temp$varperc$simple[1:n_basis], 
         simple = temp$simple[, 1:n_basis])
  })
  
  if (b == "CUB") {
    lapply(temp_sim, function(x) enframe(x$varperc, name = "SB", value = "Variance")) %>% 
      bind_rows() %>% 
      mutate(Variance = Variance/100) %>% 
      filter(SB <= 3) %>% 
      mutate(Bootstrap = rep(1:2000, each = 3)) %>% 
      select(Bootstrap, everything()) %>% 
      write_csv("data/supplementary_tables/ST10.simple_basis_variances.csv")
  }
  
  temp_varperc <- lapply(temp_sim, function(x) enframe(x$varperc, name = "SB", value = "V")) %>% 
    bind_rows() %>% 
    mutate(V = V/100) %>% 
    group_by(SB) %>% 
    group_modify(~ {
      bc_sum(.x$V) %>% 
        enframe(name = "Measure", value = "Value") %>% 
        pivot_wider(names_from = "Measure", values_from = "Value")
    }) %>% 
    ungroup() %>% 
    mutate(Basis = b) %>% 
    select(Basis, everything())
  
  temp_simp <- lapply(temp_sim, `[[`, 2L) %>% 
    abind(along = 3L)
  signs <- sapply(temp_simp[1, , 1], sign)
  for (j in 1:(dim(temp_simp)[2])) {
    for (k in 1:(dim(temp_simp)[3])) {
      if (sign(temp_simp[1, j, k]) != signs[j]) temp_simp[, j, k] <- -1*temp_simp[, j, k]
    }
  }
  
  temp_simp <- apply(temp_simp, 1:2, bc_sum)
  dimnames(temp_simp)[[3]] <- paste0("SB", 1:n_basis)
  temp_simp <- map(1:3, function(i) {
        temp_simp[i, , ] %>% 
          as_tibble(rownames = "Temperature") %>% 
          pivot_longer(-Temperature, names_to = "SB", values_to = tt[i])
      }) %>% 
    reduce(inner_join, by = c("Temperature", "SB")) %>% 
    mutate(Temperature = as.integer(Temperature), 
           Basis = b) %>% 
    select(Basis, everything())
  
  list(temp_varperc, temp_simp)
})


varperc <- map_df(sim, `[[`, 1L)
simple <- map_df(sim, `[[`, 2L)

ggplot(filter(varperc, SB <= 5), aes(x = SB, colour = Basis)) + theme_bw() + 
  geom_linerange(aes(ymin = Lower, ymax = Upper), position = position_dodge(0.6)) + 
  geom_point(aes(y = Mean), position = position_dodge(0.6)) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_colour_brewer(type = "qual", palette = "Dark2") + 
  labs(x = "Simple basis function", y = "% variance explained", colour = "")
ggsave("figures/basis_varperc_all.png", width = 6, height = 4, units = "in")

ggplot(filter(simple, SB %in% paste0("SB", 1:4)), 
       aes(x = Temperature, colour = SB)) + theme_bw() + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "grey80", alpha = 0.5) + 
  geom_line(aes(y = Mean)) + 
  facet_wrap(~ Basis, nrow = 1) + 
  scale_colour_brewer(type = "qual", palette = "Set2") + 
  labs(x = expression("Temperature ("*degree*"C)"), y = "Loading", 
       colour = "SB function")

write_csv(varperc, "data/regression_results2/simple_basis_variance.csv")
write_csv(simple, "data/regression_results2/simple_basis_functions.csv")
