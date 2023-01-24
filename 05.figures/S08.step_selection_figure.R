library(tidyverse)
library(grid)
library(gridExtra)
source("src/bc_sum.R")

source("06.figures/S08a.step_fixed_temperature.R")
source("06.figures/S08b.step_selection.R")
source("06.figures/S08c.step_clusters.R")
source("06.figures/S08d.step_cov_intercept.R")

p_fixed <- p_fixed + labs(tag = "A")
p_int <- p_int + labs(tag = "B") 
p_clusters <- p_clusters + labs(tag = "D") 
p_cov <- p_cov + labs(tag = "C")

gp <- arrangeGrob(ggplotGrob(p_fixed), ggplotGrob(p_int), ggplotGrob(p_selection), 
                  ggplotGrob(p_cov), ggplotGrob(p_clusters), 
                  layout_matrix = matrix(c(1, 1, 1, 4, 4, 4, 4, 4, 
                                           1, 1, 1, 4, 4, 4, 4, 4, 
                                           2, 3, 3, 5, 5, 5, 5, 5,  
                                           2, 3, 3, 5, 5, 5, 5, 5), 
                                         nrow = 4, byrow = TRUE))
ggsave("figures/step_selection_figure.png", gp, 
       width = 12, height = 7, units = "in")
