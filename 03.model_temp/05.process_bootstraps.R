library(tidyverse)
library(lme4)
library(parallel)


if (file.exists("process_boots.log")) system("rm process_boots.log")
system("touch process_boots.log")

# basis <- c("CUB", "LIN", "STEP")
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
    # reff <- ranef(m, condVar = TRUE)
    reff <- ranef(m, condVar = FALSE)
    
    # # For every component EXCEPT the Variety coefficients, variability estimated 
    # # from the bootstrap is sufficient. Because we estimate selection in each 
    # # bootstrap to derive the variability of the selection coefficient function, 
    # # we DO want to retrieve the bootstrap-specific reliabilities of the random 
    # # Variety coefficients.
    # rrel <- attr(reff$Variety, "postVar")
    # dimnames(rrel) <- list(names(reff$Variety), names(reff$Variety), 
    #                        attr(reff$Variety, "row.names"))
    rrel <- NULL
    
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
    
    list(feff, reff, rrel, vc)
  }, mc.cores = 100L, mc.silent = FALSE)
  
  feff <- lapply(res, `[[`, 1L) %>% bind_rows()
  reff <- lapply(res, `[[`, 2L) %>% bind_rows()
  # rrel <- lapply(res, `[[`, 3L)
  vc <- lapply(res, `[[`, 4L) %>% bind_rows()
  
  write_csv(feff, paste0("data/bootstraps2/", b, "_feffects_raw.csv"))
  write_csv(reff, paste0("data/bootstraps2/", b, "_reffects_raw.csv"))
  # write_rds(rrel, paste0("data/bootstraps/", b, "_rrel_raw.rds"))
  write_csv(vc, paste0("data/bootstraps2/", b, "_vc_raw.csv"))
}
