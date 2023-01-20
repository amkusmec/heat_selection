library(tidyverse)
library(grid)
library(gridExtra)

source("06.figures/S13a.step_simple_basis.R")
source("06.figures/S13b.step_pfa.R")
# source("06.figures/S13c.step_constraint.R")

p_sb <- p_sb + labs(tag = "A")
p_pfa <- p_pfa + labs(tag = "B")
# p_con <- p_con + labs(tag = "C")

# gp <- arrangeGrob(ggplotGrob(p_sb), ggplotGrob(p_pfa), ggplotGrob(p_con), 
#                   layout_matrix = matrix(c(1, 2, 2, 
#                                            3, 3, 3), nrow = 2, byrow = TRUE))
gp <- arrangeGrob(ggplotGrob(p_sb), ggplotGrob(p_pfa), 
                  layout_matrix = matrix(c(1, 2, 2), nrow = 1, byrow = TRUE))
ggsave("figures/revision1/step_constraint_figure.png", gp, 
       # width = 10, height = 6, units = "in")
       width = 8, height = 3.5, units = "in")
