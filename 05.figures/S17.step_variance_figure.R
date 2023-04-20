library(tidyverse)
library(abind)
library(grid)
library(gridExtra)
source("src/bc_sum.R")

source("06.figures/S17a.step_variance_all.R")
source("06.figures/S17b.step_variance_temp.R")
source("06.figures/S17c.step_cor_func.R")

p_all <- p_all + labs(tag = "A")
p_temp <- p_temp + labs(tag = "B") 
p_cor <- p_cor + labs(tag = "C")

gp <- arrangeGrob(ggplotGrob(p_all), ggplotGrob(p_temp), ggplotGrob(p_cor), 
                  layout_matrix = matrix(c(1:3, 3), nrow = 2, byrow = TRUE))
ggsave("figures/SF17_step_variance_figure.png", gp, 
       width = 6, height = 6, units = "in")
