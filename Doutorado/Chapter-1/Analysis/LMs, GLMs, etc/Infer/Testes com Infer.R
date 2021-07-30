# Tests with Infer

# Eduardo Q Marques 27-07-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggridges)
library(scales)
library(infer)

#Data ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

ndvi = df %>% 
  filter(index == "ndvi") #%>% 
 # mutate(value = value - mean(value, na.rm = T))

#2004 -------------------------------------------------------------------------------------
ndvi04 = ndvi %>% filter(year == 2004)

hist(ndvi04$value)

ggplot(ndvi04, aes(x = value, y = treat, fill = treat))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      alpha = 0.7, scale = 3, color = "white")+ 
  scale_fill_manual(values = c("orange", "red", "blue"), guide = FALSE)+
  labs(x = "NDVI", y = NULL)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank())

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


#2005 ------------------------------------------------------------------------------------
ndvi05 = ndvi %>% filter(year == 2005)

hist(ndvi05$value)

ggplot(ndvi05, aes(x = value, y = treat, fill = treat))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      alpha = 0.7, scale = 3, color = "white")+ 
  scale_fill_manual(values = c("orange", "red", "blue"), guide = FALSE)+
  labs(x = "NDVI", y = NULL)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank())

diff <- ndvi05 %>% 
  specify(value ~ treat) %>% 
  calculate(stat = "F")
diff

null_distn <- ndvi05 %>%
  specify(value ~ treat) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

visualize(null_distn) +
  shade_p_value(obs_stat = diff, direction = "greater")

null_distn %>%
  get_p_value(obs_stat = diff, direction = "greater")


#2006 ------------------------------------------------------------------------------------
ndvi06 = ndvi %>% filter(year == 2006)

hist(ndvi06$value)

ggplot(ndvi06, aes(x = value, y = treat, fill = treat))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      alpha = 0.7, scale = 3, color = "white")+ 
  scale_fill_manual(values = c("orange", "red", "blue"), guide = FALSE)+
  labs(x = "NDVI", y = NULL)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank())

diff <- ndvi06 %>% 
  specify(value ~ treat) %>% 
  calculate(stat = "F")
diff

null_distn <- ndvi06 %>%
  specify(value ~ treat) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

visualize(null_distn) +
  shade_p_value(obs_stat = diff, direction = "greater")

null_distn %>%
  get_p_value(obs_stat = diff, direction = "greater")


#2008 ------------------------------------------------------------------------------------
ndvi08 = ndvi %>% filter(year == 2008)

hist(ndvi08$value)

ggplot(ndvi08, aes(x = value, y = treat, fill = treat))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      alpha = 0.7, scale = 3, color = "white")+ 
  scale_fill_manual(values = c("orange", "red", "blue"), guide = FALSE)+
  labs(x = "NDVI", y = NULL)+
  theme_minimal()+
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


#2010 ------------------------------------------------------------------------------------
ndvi10 = ndvi %>% filter(year == 2010)

hist(ndvi10$value)

ggplot(ndvi10, aes(x = value, y = treat, fill = treat))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      alpha = 0.7, scale = 3, color = "white")+ 
  scale_fill_manual(values = c("orange", "red", "blue"), guide = FALSE)+
  labs(x = "NDVI", y = NULL)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank())

diff <- ndvi10 %>% 
  specify(value ~ treat) %>% 
  calculate(stat = "F")
diff

null_distn <- ndvi10 %>%
  specify(value ~ treat) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

visualize(null_distn) +
  shade_p_value(obs_stat = diff, direction = "greater")

null_distn %>%
  get_p_value(obs_stat = diff, direction = "greater")


#2011 ------------------------------------------------------------------------------------
ndvi11 = ndvi %>% filter(year == 2011)

hist(ndvi11$value)

ggplot(ndvi11, aes(x = value, y = treat, fill = treat))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      alpha = 0.7, scale = 3, color = "white")+ 
  scale_fill_manual(values = c("orange", "red", "blue"), guide = FALSE)+
  labs(x = "NDVI", y = NULL)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank())

diff <- ndvi11 %>% 
  specify(value ~ treat) %>% 
  calculate(stat = "F")
diff

null_distn <- ndvi11 %>%
  specify(value ~ treat) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1100, type = "permute") %>%
  calculate(stat = "F")

visualize(null_distn) +
  shade_p_value(obs_stat = diff, direction = "greater")

null_distn %>%
  get_p_value(obs_stat = diff, direction = "greater")


#2012 ------------------------------------------------------------------------------------
ndvi12 = ndvi %>% filter(year == 2012)

hist(ndvi12$value)

ggplot(ndvi12, aes(x = value, y = treat, fill = treat))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      alpha = 0.7, scale = 3, color = "white")+ 
  scale_fill_manual(values = c("orange", "red", "blue"), guide = FALSE)+
  labs(x = "NDVI", y = NULL)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank())

diff <- ndvi12 %>% 
  specify(value ~ treat) %>% 
  calculate(stat = "F")
diff

null_distn <- ndvi12 %>%
  specify(value ~ treat) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1200, type = "permute") %>%
  calculate(stat = "F")

visualize(null_distn) +
  shade_p_value(obs_stat = diff, direction = "greater")

null_distn %>%
  get_p_value(obs_stat = diff, direction = "greater")












