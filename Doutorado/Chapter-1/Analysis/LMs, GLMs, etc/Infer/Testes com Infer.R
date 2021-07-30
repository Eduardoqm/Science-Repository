# Tests with Infer

# Eduardo Q Marques 27-07-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggridges)
library(scales)

#Data ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

ndvi = df %>% 
  filter(index == "ndvi") #%>% 
 # mutate(value = value - mean(value, na.rm = T))

#2004 -------------------------------------------------------------------------------------
ndvi04 = ndvi %>% filter(year == 2004)

hist(ndvi04$value)

ggplot(ndvi04, aes(x = value, y = treat, fill = treat)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, scale = 3, color = "white") + 
  scale_fill_manual(values = c("orange", "red", "blue"), guide = FALSE) + 
  #scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "NDVI", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


library(infer)

diff <- ndvi04 %>% 
  specify(value ~ treat) %>% 
  calculate(stat = "F")
diff

null_distn <- ndvi04 %>%
  specify(value ~ treat) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

visualize(null_distn) +
  shade_p_value(obs_stat = diff, direction = "greater")

null_distn %>%
  get_p_value(obs_stat = diff, direction = "greater")


#2008 -------------------------------------------------------------------------------------
ndvi08 = ndvi %>% filter(year == 2008)

hist(ndvi08$value)

ggplot(ndvi08, aes(x = value, y = treat, fill = treat)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, scale = 3, color = "white") + 
  scale_fill_manual(values = c("orange", "red", "blue"), guide = FALSE) + 
  #scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "NDVI", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


diff <- ndvi08 %>% 
  specify(value ~ treat) %>% 
  calculate(stat = "F")
diff

null_distn <- ndvi08 %>%
  specify(value ~ treat) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

visualize(null_distn) +
  shade_p_value(obs_stat = diff, direction = "greater")

null_distn %>%
  get_p_value(obs_stat = diff, direction = "greater")



