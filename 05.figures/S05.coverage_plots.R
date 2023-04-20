library(tidyverse)
library(grid)
library(gridExtra)

temp <- read_csv("data/strict/temperature_noI.csv") %>% 
  select(-Fixed) %>% 
  pivot_wider(names_from = "Temperature", values_from = "Variable")

pct_exp <- apply(temp[, -(1:3)], 2, function(x) sum(x > 0))

reg_dat <- read_rds("data/strict/regression_data.rds") %>% 
  mutate(Trial = paste(Year, County, sep = "_"))
temp <- temp %>% 
  pivot_longer(`-1`:`41`, names_to = "Temperature", values_to = "Time") %>% 
  mutate(Temperature = as.integer(Temperature), 
         Trial = paste(Year, County, sep = "_"))

exp_obs <- tibble(Temperature = -1:41) %>% 
  mutate(N = map_int(Temperature, function(tt) {
    tr <- temp %>% 
      filter(Temperature == tt) %>% 
      filter(Time > 0) %>% 
      pull(Trial)
    reg_dat %>% 
      filter(Trial %in% tr) %>% 
      nrow()
  }))

exp_var <- tibble(Temperature = -1:41) %>% 
  mutate(N = map_int(Temperature, function(tt) {
    tr <- temp %>% 
      filter(Temperature == tt) %>% 
      filter(Time > 0) %>% 
      pull(Trial)
    reg_dat %>% 
      filter(Trial %in% tr) %>% 
      pull(Variety) %>% 
      unique() %>% 
      length()
  }))

exp_cty <- tibble(Temperature = -1:41) %>% 
  mutate(N = map_int(Temperature, function(tt) {
    tr <- temp %>% 
      filter(Temperature == tt) %>% 
      filter(Time > 0) %>% 
      pull(Trial)
    reg_dat %>% 
      filter(Trial %in% tr) %>% 
      pull(County) %>% 
      unique() %>% 
      length()
  }))

exp_yr <- tibble(Temperature = -1:41) %>% 
  mutate(N = map_int(Temperature, function(tt) {
    tr <- temp %>% 
      filter(Temperature == tt) %>% 
      filter(Time > 0) %>% 
      pull(Trial)
    reg_dat %>% 
      filter(Trial %in% tr) %>% 
      pull(Year) %>% 
      unique() %>% 
      length()
  }))

pC <- ggplot(enframe(pct_exp) %>% mutate(name = as.integer(name))) + theme_classic() + 
  geom_point(aes(x = name, y = value), shape = 21, colour = "black", 
             fill = "#bebada", size = 3, stroke = 1) + 
  scale_y_continuous(limits = c(-1, 2600), breaks = seq(0, 2500, 500), labels = scales::label_comma()) + 
  scale_x_continuous(breaks = seq(0, 40, 5)) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")),
       y = "# trials (max. 2,581)", tag = "C") + 
  theme(axis.title = element_text(face = "bold"), 
        panel.grid.major.y = element_line(colour = "grey70")) + 
  annotate("text", x = 36, y = 200, label = "191", fontface = "bold") + 
  annotate("segment", x = 37.5, xend = 40.5, y = 200, yend = 191, 
           arrow = arrow(length = unit(0.05, "npc")))

pE <- ggplot(exp_obs) + theme_classic() + 
  geom_point(aes(x = Temperature, y = N), shape = 21, colour = "black", 
             fill = "#80b1d3", size = 3, stroke = 1) + 
  scale_y_continuous(breaks = seq(0, 175000, 25000), labels = scales::label_comma()) +
  scale_x_continuous(breaks = seq(0, 40, 5)) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")),
       y = "# observations (max. 175,805)", tag = "E") + 
  theme(axis.title = element_text(face = "bold"), 
        panel.grid.major.y = element_line(colour = "grey70")) + 
  annotate("text", x = 36, y = 7000, label = "6,355", fontface = "bold") + 
  annotate("segment", x = 38, xend = 40.5, y = 7000, yend = 6355, 
           arrow = arrow(length = unit(0.05, "npc")))


pD <- ggplot(exp_var) + theme_classic() + 
  geom_point(aes(x = Temperature, y = N), shape = 21, colour = "black", 
             fill = "#fb8072", size = 3, stroke = 1) + 
  scale_y_continuous(limits = c(-1, 4800), labels = scales::label_comma()) +
  scale_x_continuous(breaks = seq(0, 40, 5)) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")),
       y = "# varieties (max. 4,730)", tag = "D") + 
  theme(axis.title = element_text(face = "bold"), 
        panel.grid.major.y = element_line(colour = "grey70"))

pA <- ggplot(exp_cty) + theme_classic() + 
  geom_point(aes(x = Temperature, y = N), shape = 21, colour = "black", 
             fill = "#8dd3c7", size = 3, stroke = 1) + 
  scale_y_continuous(limits = c(-1, 180), labels = scales::label_comma()) +
  scale_x_continuous(breaks = seq(0, 40, 5)) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")),
       y = "# counties (max. 172)", tag = "A") + 
  theme(axis.title = element_text(face = "bold"), 
        panel.grid.major.y = element_line(colour = "grey70"))

pB <- ggplot(exp_yr) + theme_classic() + 
  geom_point(aes(x = Temperature, y = N), shape = 21, colour = "black", 
             fill = "#ffffb3", size = 3, stroke = 1) + 
  scale_y_continuous(limits = c(-1, 90), labels = scales::label_comma(), 
                     breaks = seq(0, 80, 20)) +
  scale_x_continuous(breaks = seq(0, 40, 5)) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")),
       y = "# years (max. 81)", tag = "B") + 
  theme(axis.title = element_text(face = "bold"), 
        panel.grid.major.y = element_line(colour = "grey70"))


gp <- arrangeGrob(ggplotGrob(pA), ggplotGrob(pB), ggplotGrob(pC), 
                  ggplotGrob(pD), ggplotGrob(pE), 
                  layout_matrix = matrix(1:6, nrow = 2, byrow = TRUE))
ggsave("figures/SF05_data_coverage.png", gp, 
       width = 12, height = 6, units = "in")
