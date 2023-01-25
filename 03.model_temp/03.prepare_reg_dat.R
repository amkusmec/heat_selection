library(tidyverse)
library(splines2)
library(fda)
source("src/block_bootstrap.R")


# Load the regression data ------------------------------------------------
reg_dat <- read_rds("data/strict/regression_data.rds")


# Construct different B-spline bases for temperature ----------------------
TEMP_eval <- reg_dat$Lt[[1]]
TEMP_range <- range(TEMP_eval)

HOURS <- reg_dat$Ly %>%
  simplify2array()
HOURS_c <- HOURS - matrix(rowMeans(HOURS), nrow(HOURS), ncol(HOURS), byrow = FALSE)

# List of spline parameters
#  - STEP = piece-wise constant with knots at 3 degree intervals
#  - CUB  = piece-wise cubic with optimal knots
basis_par <- list(
    "STEP" = list("Order" = 0L, "Knots" = seq(TEMP_range[1], TEMP_range[2], 3)), 
    "CUB"  = list("Order" = 3L, "Knots" = quantile(TEMP_eval, seq(0, 1, length.out = 5L)))
  )

# Compute the regression spline coefficients for the exposure functions projected 
# onto each basis:
smooths <- lapply(basis_par, function(sp) {
    H <- bSpline(TEMP_eval, knots = sp$Knots[2:(length(sp$Knots) - 1)], 
                 degree = sp$Order, intercept = FALSE)
    
    bs_obj <- basisfd(type = "bspline", rangeval = TEMP_range, nbasis = ncol(H), 
                      params = attr(H, "knots"), 
                      basisvalues = list(list(args = TEMP_eval, values = H)))
    if (sp$Order > 0L) {
      Q <- inprod(bs_obj, bs_obj, 0, 0)
    } else {
      Q <- crossprod(H)
    }
    
    a <- tcrossprod(solve(crossprod(H)), H) %*% HOURS_c
    c_mat <- t(a) %*% t(Q)
    colnames(c_mat) <- paste0("bspl", sp$Order, ".", 1:ncol(c_mat))
    c_mat
  })


# Assemble and save the data objects --------------------------------------
for (i in seq_along(smooths)) {
  bind_cols(reg_dat, smooths[[i]]) %>% 
    mutate(across(contains("bspl") | contains("PRCP") | contains("Year"), scale)) %>% 
    dplyr::select(Year:TotalHours, Longitude:Latitude, PDAY, starts_with("bspl")) %>% 
    write_rds(file = paste0("data/regression_data/reg_dat_", names(smooths)[i], ".rds"))
}


# Generate block bootstraps -----------------------------------------------
set.seed(275207)
boots <- block_bootstrap(nrow(reg_dat), grps = reg_dat$Year, n_boots = 2000)

# For convenience, replace the first bootstrap with indices for the full data
boots[, 1] <- 1:nrow(reg_dat)
write_rds(boots, "data/regression_data/block_bootstraps.rds")

# Number of unique years in each bootstrap
n_years <- apply(boots, 2, function(x) reg_dat$Year[x] %>% unique() %>% length()) %>% 
  table() %>% 
  enframe(name = "Years", value = "Count") %>% 
  mutate(Years = as.integer(Years), 
         Count = as.vector(Count))

ggplot(n_years) + theme_bw() + 
  geom_col(aes(x = Years, y = Count), colour = "black", fill = "orange", 
           alpha = 0.8) + 
  labs(x = "# unique years", y = "# bootstraps")
ggsave("figures/bootstrap_years.png", width = 6, height = 4, units = "in")
