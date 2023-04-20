library(tidyverse)
library(grid)
library(gridExtra)

temp_time <- read_csv("data/strict/exposure_trends_all.csv")
trials_time <- read_csv("data/strict/date_trends_all.csv")


pB <- ggplot(temp_time, aes(x = Temperature, y = County)) + theme_classic() + 
  geom_tile(aes(fill = as.character(Sign), alpha = Sig)) + 
  geom_vline(xintercept = c(10, 30, 36), linetype = 2, colour = "black") + 
  scale_fill_manual(values = c("-1" = "blue", "0" = "white", "1" = "red"), 
                    labels = c("Less exposure", "No change", "More exposure")) + 
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.25)) + 
  facet_wrap(~ State, scales = "free_y", 
             labeller = as_labeller(c("ILLINOIS" = "Illinois (IL) [33/41]", 
                                      "IOWA" = "Iowa (IA) [52/56]", 
                                      "KANSAS" = "Kansas (KS) [26/35]", 
                                      "NEBRASKA" = "Nebraska (NE) [27/40]"))) + 
  guides(alpha = "none") + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), 
       fill = "d exposure/yr", tag = "B") + 
  theme(axis.text.y = element_blank(), 
        axis.title = element_text(face = "bold"), 
        axis.ticks.y = element_blank(), 
        strip.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"))

pA <- ggplot(trials_time, aes(x = County, y = Variable)) + theme_classic() + 
  geom_tile(aes(fill = as.character(Sign), alpha = Sig)) + 
  scale_fill_manual(values = c("-1" = "blue", "0" = "white", "1" = "red"), 
                    labels = c("Earlier/shorter", "No change", "Later/longer")) + 
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.25)) + 
  scale_y_discrete(labels = c("Growing season\nlength", "Harvest day", "Planting day")) + 
  facet_wrap(~ State, scales = "free_x", 
             labeller = as_labeller(c("ILLINOIS" = "Illinois (IL) [33/41]", 
                                      "IOWA" = "Iowa (IA) [52/56]", 
                                      "KANSAS" = "Kansas (KS) [26/35]", 
                                      "NEBRASKA" = "Nebraska (NE) [27/40]"))) + 
  guides(alpha = "none") + 
  labs(x = "County", y = "", fill = "d/yr", tag = "A") + 
  theme(axis.text.x = element_blank(), 
        axis.title = element_text(face = "bold"), 
        axis.ticks.x = element_blank(), 
        strip.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"))

gp <- arrangeGrob(ggplotGrob(pA), ggplotGrob(pB), 
                  layout_matrix = matrix(1:2, ncol = 1))
ggsave("figures/revision2/SF14_trial_phenology.png", gp, width = 8, height = 8, units = "in")
