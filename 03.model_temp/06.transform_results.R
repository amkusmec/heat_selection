library(tidyverse)
library(abind)
library(splines2)


# List of spline parameters
#  - STEP = piece-wise constant with knots at 3 degree intervals
#  - CUB  = piece-wise cubic with optimal knots
TEMP_eval <- -1:41
TEMP_range <- range(TEMP_eval)
basis_par <- list(
  "STEP" = list("Order" = 0L, "Knots" = seq(TEMP_range[1], TEMP_range[2], 3)), 
  "CUB"  = list("Order" = 3L, "Knots" = quantile(TEMP_eval, seq(0, 1, length.out = 5L)))
)


for (b in names(basis_par)) {
  # Load the regression data frame and get the scaling factors
  reg_dat <- paste0("data/regression_data/reg_dat_", b, ".rds") %>% read_rds()
  bn <- names(reg_dat)[str_detect(names(reg_dat), "bspl")]
  rms <- sapply(bn, function(x) attr(reg_dat[[x]], "scaled:scale"))
  
  # Reconstruct the basis matrix
  H <- bSpline(TEMP_eval, knots = basis_par[[b]]$Knots[2:(length(basis_par[[b]]$Knots) - 1)], 
               degree = basis_par[[b]]$Order, intercept = FALSE)
  Ha <- rbind(matrix(c(1, rep(0, ncol(H))), nrow = 1),
              cbind(rep(0, length(TEMP_eval)), H))
  
  ### Fixed temperature effects
  feff <- paste0("data/bootstraps2/", b, "_feffects_raw.csv") %>% read_csv()
  
  temp_fix <- feff %>% 
    filter(str_detect(Coefficient, "bspl")) %>% 
    pivot_wider(names_from = "Coefficient", values_from = "Value") %>% 
    dplyr::select(-Bootstrap) %>% 
    as.matrix()
  temp_fix <- temp_fix/matrix(rms, nrow = nrow(temp_fix), 
                              ncol = ncol(temp_fix), byrow = TRUE)
  
  beta <- H %*% t(temp_fix)
  write_rds(beta, file = paste0("data/regression_results2/", b, "_results/", 
                                b, "_temperature_fixed.rds"))
  
  ### Precipitation fixed effects
  prcp_fix <- feff %>% 
    filter(str_detect(Coefficient, "PRCP")) %>% 
    pivot_wider(names_from = "Coefficient", values_from = "Value") %>% 
    dplyr::select(-Bootstrap) %>% 
    as.matrix()
  prcp_fix <- prcp_fix/attr(reg_dat[["PRCP"]], "scaled:scale")
  
  PRCP_eval <- attr(reg_dat[["PRCP"]], "scaled:scale")*
    seq(min(reg_dat$PRCP), max(reg_dat$PRCP), length.out = 100)
  HP <- bSpline(PRCP_eval, df = ncol(prcp_fix))
  prcp_func <- HP %*% t(prcp_fix)
  write_rds(prcp_func, file = paste0("data/regression_results2/", b, "_results/", 
                                     b, "_precipitation_fixed.rds"))
  
  pday <- feff %>% 
    filter(Coefficient == "PDAY")
  write_rds(pday, file = paste0("data/regression_results2/", b, "_results/", 
                                b, "_pday.rds"))
  
  # Intermediate clean-up
  rm(beta, feff, HP, prcp_fix, prcp_func, temp_fix, PRCP_eval, pday); gc()
  
  ### Random temperature effects
  reff <- paste0("data/bootstraps2/", b, "_reffects_raw.csv") %>% read_csv()
  
  varieties <- unique(reg_dat$Variety) %>% sort()
  temp_rand <- reff %>% 
    filter(grpvar == "Variety") %>% 
    dplyr::select(-grpvar) %>% 
    mutate(term = str_remove_all(term, "[\\(\\)]")) %>% 
    pivot_wider(names_from = "term", values_from = "condval") %>% 
    split(.$Bootstrap) %>% 
    map(function(df) {
        mm <- as.matrix(df[, -(1:2)])
        dimnames(mm) <- list(df$grp, names(df)[-(1:2)])
        
        ms <- setdiff(varieties, rownames(mm))
        mm <- rbind(mm, 
                    matrix(as.numeric(NA), nrow = length(ms), 
                           ncol = ncol(mm), dimnames = list(ms, colnames(mm))))
        mm <- mm[order(rownames(mm)), ]
        mm/matrix(c(1, rms), nrow = nrow(mm), ncol = ncol(mm), byrow = TRUE)
      }) %>% 
    abind(along = 3)
  
  B <- map(1:(dim(temp_rand)[3]), function(i) Ha %*% t(temp_rand[, , i])) %>% 
    abind(along = 3)
  write_rds(B, file = paste0("data/regression_results2/", b, "_results/", 
                             b, "_temperature_random.rds"))
  
  # Intermediate clean-up
  rm(temp_rand, B); gc()
  
  ### Random county effects
  county <- reff %>% 
    filter(str_detect(grpvar, "County")) %>% 
    mutate(grp = paste0("County", grp)) %>% 
    pivot_wider(names_from = "grp", values_from = "condval", 
                values_fill = as.numeric(NA)) %>% 
    dplyr::select(-Bootstrap, -grpvar, -term) %>% 
    as.matrix()
  write_rds(county, file = paste0("data/regression_results2/", b, "_results/", 
                                  b, "_county_random.rds"))
  
  ### Random year effects
  years <- reff %>% 
    filter(str_detect(grpvar, "State"))
  deg <- length(unique(years$term))
  years <- years %>% 
    arrange(Bootstrap, grp, term) %>% 
    mutate(term = str_replace(term, "\\([A-Za-z0-9 ,=]*\\)", paste0(deg, ".")), 
           grp = paste(term, grp, sep = ":")) %>% 
    dplyr::select(-grpvar, -term) %>% 
    pivot_wider(names_from = "grp", values_from = "condval", 
                values_fill = as.numeric(NA)) %>% 
    dplyr::select(-Bootstrap) %>% 
    as.matrix()
  write_rds(years, file = paste0("data/regression_results2/", b, "_results/", 
                                 b, "_year_random.rds"))
  
  # Intermediate clean-up
  rm(county, reff, years, deg); gc()
  
  ### Variance components
  vc <- paste0("data/bootstraps2/", b, "_vc_raw.csv") %>% 
    read_csv() %>% 
    filter(grp == "Variety") %>% 
    dplyr::select(-grp) %>% 
    pivot_wider(names_from = "var2", values_from = "vcov", 
                values_fill = as.numeric(NA)) %>% 
    split(.$Bootstrap) %>% 
    map(function(df) {
        v <- as.matrix(df[, -(1:2)])
        rownames(v) <- colnames(v)
        v[lower.tri(v, diag = FALSE)] <- t(v)[lower.tri(v, diag = FALSE)]
        Ha %*% v %*% t(Ha)
      }) %>% 
    abind(along = 3)
  dimnames(vc) <- list(c("Intercept", TEMP_eval), c("Intercept", TEMP_eval), NULL)
  write_rds(vc, file = paste0("data/regression_results2/", b, "_results/", 
                              b, "_temperature_vc.rds"))
}
