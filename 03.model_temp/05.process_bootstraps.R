library(tidyverse)
library(lme4)
library(parallel)


if (file.exists("process_boots.log")) system("rm process_boots.log")
system("touch process_boots.log")

basis <- c("CUB", "STEP")

for (b in basis) {
  files <- list.files(paste0("data/bootstraps2/", b, "_bootstraps"), 
                      "*\\.rds", full.names = TRUE)
  
  res <- mclapply(files, function(f) {
    cat(f, "\n", file = "process_boots.log", append = TRUE)
    
    m <- read_rds(f)
    
    # Retrieve the fixed effects
    feff <- fixef(m) %>% 
      enframe(name = "Coefficient", value = "Value") %>% 
      mutate(Bootstrap = str_extract(f, "[0-9]{4}") %>% as.integer()) %>% 
      dplyr::select(Bootstrap, everything())
    
    ### Retrieve the random effects
    reff <- ranef(m, condVar = FALSE)
    
    reff <- as.data.frame(reff) %>% 
      mutate(across(term:grp, as.character)) %>% 
      mutate(Bootstrap = str_extract(f, "[0-9]{4}") %>% as.integer()) %>% 
      dplyr::select(Bootstrap, grpvar:condval)
    
    # Retrieve the variance components for each random effect
    vc <- VarCorr(m) %>% 
      as_tibble() %>% 
      mutate(var1 = str_remove_all(var1, "[\\(\\)]"), 
             var2 = if_else(is.na(var2), var1, var2), 
             Bootstrap = str_extract(f, "[0-9]{4}") %>% as.integer()) %>%
      select(Bootstrap, grp:vcov)
    
    list(feff, reff, vc)
  }, mc.cores = 100L, mc.silent = FALSE)
  
  feff <- lapply(res, `[[`, 1L) %>% bind_rows()
  reff <- lapply(res, `[[`, 2L) %>% bind_rows()
  vc <- lapply(res, `[[`, 3L) %>% bind_rows()
  
  write_csv(feff, paste0("data/bootstraps2/", b, "_feffects_raw.csv"))
  write_csv(reff, paste0("data/bootstraps2/", b, "_reffects_raw.csv"))
  write_csv(vc, paste0("data/bootstraps2/", b, "_vc_raw.csv"))
}
