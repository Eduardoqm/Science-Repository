#Elias Scenary Graphs
#Author: Fernando ELias
#Updated by Eduardo Q Marques 27-10-2026

### Figuras dos cenarios
library(tidyverse)
library(patchwork)
require(ggpubr)

df <- tibble(
  Scenario = factor(
    c(
      "SF → Pasture",
      "Pasture → SF",
      "Young SF protected\nfor 20 years\n(38-year-old SF)"
    ),
    levels = c(
      "SF → Pasture",
      "Pasture → SF",
      "Young SF protected\nfor 20 years\n(38-year-old SF)"
    )
  ),
  LST = c(
    0.05,
    -0.65,
    -0.90
  ),
  ETm3 = c(
    -7.42,
    11.13,
    20.4 
  ),
  ET = c(
    -0.01,
    0.06,
    0.11
  ),
  AGB = c(
    -0.03223,
    0.962,
    1.397
  )
)

#---------------------------------
# Colors
#---------------------------------

cols <- c(
  "SF → Pasture" = "#C65A5A",
  "Pasture → SF" = "#66BD63",
  "Young SF protected\nfor 20 years\n(38-year-old SF)" = "#1A9850"
)

#---------------------------------
# Panel A - LST
#---------------------------------

p1 <- ggplot(df, aes(x = Scenario,
                     y = LST,
                     fill = Scenario)) +
  
  geom_col(width = 0.7) +
  
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             colour = "grey40") +
  
  geom_text(
    aes(label = sprintf("%.2f", LST)),
    hjust = ifelse(df$LST > 0, -0.25, 1.20),
    size = 4.5) +
  
  coord_flip() +
  
  scale_fill_manual(values = cols) +
  
  scale_y_continuous(
    limits = c(-1.0, 0.15),
    breaks = seq(-1.0, 0.1, by = 0.2),
    expand = expansion(mult = c(0.02, 0.08))) +
  
  labs(
    x = NULL,
    y = expression("LST (°C)")) +
  
  theme_classic(base_size = 16) +
  
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank())

#---------------------------------
# Panel B - ET
#---------------------------------

p2 <- ggplot(df, aes(x = Scenario,
                     y = ET,
                     fill = Scenario)) +
  
  geom_col(width = 0.7) +
  
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             colour = "grey40") +
  
  geom_text(aes(label = sprintf("%.2f", ET)),
            hjust = ifelse(df$ET > 0, -0.25, 1.20),
            size = 4.5) +
  
  coord_flip() +
  
  scale_fill_manual(values = cols) +
  
  scale_y_continuous(
    limits = c(-0.06, 0.13),
    breaks = seq(-0.06, 0.12, by = 0.03),
    expand = expansion(mult = c(0.02, 0.08))) +
  
  labs(
    x = NULL,
    y = expression("ET (mm day"^{-1}*")")) +
  
  theme_classic(base_size = 16) +
  
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank())

#---------------------------------
# Panel C - ABG
#---------------------------------

p3 <- ggplot(df, aes(x = Scenario,
                     y = AGB,
                     fill = Scenario)) +
  
  geom_col(width = 0.7) +
  
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             colour = "grey40") +
  
  geom_text(aes(label = sprintf("%.2f", AGB)),
            hjust = ifelse(df$AGB > 0, -0.25, 1.20),
            size = 4.5) +
  
  coord_flip() +
  
  scale_fill_manual(values = cols) +
  
  scale_y_continuous(limits = c(-0.1, 1.7),
                     #breaks = seq(-0.06, 0.12, by = 0.03),
                     #expand = expansion(mult = c(0.02, 0.08))
  ) +
  
  labs(
    x = NULL,
    y = expression("AGB (Pg)")) +
  
  theme_classic(base_size = 16) +
  
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank())
#---------------------------------
# Combine panels
#---------------------------------
fig <- ggarrange(p1, p2, p3, 
                 ncol = 1,
                 nrow = 3,
                 labels = c('a)', 'b)', 'c)'),   
                 label.x = 0.01, 
                 align = c("v"),
                 common.legend = F); fig

#Version with Cubic metrics ----------------------------------------------------
#---------------------------------
# Panel B - ET
#---------------------------------

p2b <- ggplot(df, aes(x = Scenario,
                      y = ETm3,
                      fill = Scenario)) +
  
  geom_col(width = 0.7) +
  
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             colour = "grey40") +
  
  geom_text(aes(label = sprintf("%.2f", ETm3)),
            hjust = ifelse(df$ET > 0, -0.25, 1.20),
            size = 4.5) +
  
  coord_flip() +
  
  scale_fill_manual(values = cols) +
  
  scale_y_continuous(
    limits = c(-10, 21),
    expand = expansion(mult = c(0.02, 0.08))) +
  
  labs(
    x = NULL,
    y = expression("ET (billion m³ year"^{-1}*")")) +
  
  theme_classic(base_size = 16) +
  
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank()) ;p2b


fig2 <- ggarrange(p1, p2b, p3, 
                  ncol = 1,
                  nrow = 3,
                  labels = c('a)', 'b)', 'c)'),   
                  label.x = 0.01, 
                  align = c("v"),
                  common.legend = F); fig2
