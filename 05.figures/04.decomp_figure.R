library(tidyverse)
library(grid)
library(gridExtra)

source("06.figures/04a.simple_basis.R")
source("06.figures/04b.pfa.R")
# source("06.figures/04c.constraint.R")

p_sb <- p_sb + labs(tag = "A")
p_pfa <- p_pfa + labs(tag = "B")
# p_con <- p_con + labs(tag = "C")

# gp <- arrangeGrob(ggplotGrob(p_sb), ggplotGrob(p_pfa), ggplotGrob(p_con), 
#                   layout_matrix = matrix(c(1, 2, 2, 
#                                            NA, 3, 3), nrow = 2, byrow = TRUE))
gp <- arrangeGrob(ggplotGrob(p_sb), ggplotGrob(p_pfa), 
                  layout_matrix = matrix(c(1, 2, 2), nrow = 1, byrow = TRUE))
ggsave("figures/constraint_figure.png", gp, 
       # width = 9, height = 6, units = "in")
       width = 8, height = 3.5, units = "in")

       