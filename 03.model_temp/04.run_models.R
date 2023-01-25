library(tidyverse)
library(splines)
library(lme4)
library(parallel)


if (file.exists("final_boots.log")) system("rm final_boots.log")
system("touch final_boots.log")

boots <- read_rds("data/regression_data/block_bootstraps.rds")
basis <- c("CUB", "STEP")

for (b in basis) {
  reg_dat <- paste0("data/regression_data/reg_dat_", b, ".rds") %>% read_rds()
  bn <- names(reg_dat)[str_detect(names(reg_dat), "bspl")]
  
  # Set up formulae
  fix_form <- paste0("LogYield ~ 1 + bs(PRCP, df = 5) + PDAY + ", 
                     paste(bn, collapse = " + "))
  rand_form <- paste0("(1 | County) + (0 + poly(Year, degree = 2, simple = TRUE) | State) + ", 
                      "(1 + ", paste(bn, collapse = " + "), " | Variety)")
  
  # Bootstrap loop
  res <- mclapply(1:ncol(boots), function(i) {
      cat(b, "-- Bootstrap", i, "\n", 
          file = "final_boots.log", append = TRUE)
      model <- lmer(as.formula(paste(fix_form, rand_form, sep = " + ")), 
                    data = reg_dat[boots[, i], ], REML = FALSE, 
                    control = lmerControl(optCtrl = list(maxeval = 500), 
                                          calc.derivs = FALSE))
      
      stem <- ifelse(i < 10, 
                     paste0("000", i), 
                     ifelse(i < 100, paste0("00", i), 
                            ifelse(i < 1000, paste0("0", i), i)))
      saveRDS(model, paste0("data/bootstraps2/", b, 
                            "_bootstraps/bootstrap_", stem, ".rds"))
      
      TRUE
    }, mc.cores = detectCores(), mc.preschedule = FALSE, mc.silent = FALSE)
}
