library(tidyverse)
source("src/bc_sum.R")


mday <- read_csv("data/strict/trials_noI.csv") %>% 
  mutate(PDAY = lubridate::yday(Planted)) %>% 
  pull(PDAY) %>% 
  median()

basis <- c("CUB", "STEP")
pday <- map_df(basis, function(b) {
  map_df(c(0.95, 0.9), function(a) {
    paste0("data/regression_results2/", b, "_results/", b, "_pday.rds") %>% 
      read_rds() %>% 
      pull(Value) %>% 
      exp() %>% 
      `-`(1) %>% 
      bc_sum(alpha = a) %>% 
      enframe(name = "Measure", value = "Value") %>% 
      mutate(Basis = b, 
             Alpha = a) %>% 
      pivot_wider(names_from = "Measure", values_from = "Value")
  })
})

ggplot(pday, aes(x = Basis)) + theme_classic() + 
  geom_linerange(aes(ymin = Lower, ymax = Upper), filter(pday, Alpha == 0.95), 
                 size = 0.75) + 
  geom_linerange(aes(ymin = Lower, ymax = Upper), filter(pday, Alpha == 0.9), 
                 size = 1.5) + 
  geom_point(aes(y = Mean), size = 4) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "red") + 
  scale_y_continuous(labels = scales::label_percent()) + 
  scale_x_discrete(labels = c("Cubic", "Constant")) + 
  labs(x = "", y = "% yield/Julian day") + 
  theme(axis.title = element_text(face = "bold"))
ggsave("figures/SF07_planting_date.png", width = 3, height = 2, units = "in")
