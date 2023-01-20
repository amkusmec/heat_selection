# Function to generate block bootstraps -----------------------------------
# Modified from: [https://www.baseballprospectus.com/news/article/38289/bayesian-bagging-generate-uncertainty-intervals-catcher-framing-story/]
# Input:
#    - size (integer/numeric)  = sample size of the dataset
#    - grps (any)              = vector of length size providing group labels
#                                for each sample in the original dataset
#    - n_boots (default: 1e4)  = number of resamples
#    - verbose (default: TRUE) = print progress updates
#
# Output:
#    - boot = integer matrix of dimension (size x n_boots); each column contains
#             the indices of a bootstrap resample
block_bootstrap <- function(size, grps, n_boots = 1e4, verbose = TRUE) {
  boots <- matrix(0L, nrow = size, ncol = n_boots)
  
  for (i in 1:n_boots) {
    if (verbose) cat(i, "/", n_boots, "\r")
    
    # Accumulated samples
    sample_idx <- integer(0)
    
    # Get unique group IDs and the number of sampled groups
    un_grps <- unique(grps)
    sample_max <- length(un_grps)
    
    # Accumulate sampled groups until the total sample size meets or exceeds 
    # the original sample size
    repeat {
      if (length(sample_idx) >= size) break
      
      sample_weights <- rexp(sample_max, 1)
      sample_weights <- sample_weights/sum(sample_weights)
      
      grp_samples <- sample(un_grps, size = sample_max, prob = sample_weights, 
                            replace = TRUE)
      idx <- which(grps %in% grp_samples)
      sample_idx <- append(sample_idx, idx)
    }
    
    boots[, i] <- sample_idx[1:size]
  }
  
  boots
}
