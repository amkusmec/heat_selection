library(tidyverse)
library(grid)
library(gridExtra)

source("06.figures/01a.map_figure.R")
source("06.figures/01b.state_trends.R")
source("06.figures/01c.average_exposure.R")
source("06.figures/01d.exposure_cdf.R")

p_map <- p_map + labs(tag = "A")
p_trends <- p_trends + labs(tag = "B")
p_exposure <- p_exposure + labs(tag = "C") 
p_cdf <- p_cdf + labs(tag = "D")

gp <- arrangeGrob(ggplotGrob(p_map), ggplotGrob(p_trends), 
                  ggplotGrob(p_exposure), ggplotGrob(p_cdf), 
                  layout_matrix = matrix(c(1, 1, 1, 1, 2, 2, 2, 2, 
                                           3, 3, 4, 4, 2, 2, 2, 2), 
                                         nrow = 2, byrow = TRUE))
ggsave("figures/overview_figure.png", gp, 
       width = 16, height = 8, units = "in")
