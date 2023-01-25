library(tidyverse)
library(grid)
library(gridExtra)

source("06.figures/04a.simple_basis.R")
source("06.figures/04b.pfa.R")

p_sb <- p_sb + labs(tag = "A")
p_pfa <- p_pfa + labs(tag = "B")

gp <- arrangeGrob(ggplotGrob(p_sb), ggplotGrob(p_pfa), 
                  layout_matrix = matrix(c(1, 2, 2), nrow = 1, byrow = TRUE))
ggsave("figures/constraint_figure.png", gp, 
       width = 8, height = 3.5, units = "in")

       