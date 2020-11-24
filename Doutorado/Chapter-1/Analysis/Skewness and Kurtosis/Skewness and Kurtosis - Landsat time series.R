#########################################################
# Skewness and Kurtosis of Vegetation Indices (Landsat) #
#                                                       #
# Eduardo Q Marques 24-11-2020                          #
#########################################################

library(ggplot2)
library(ggridges)
library(e1071)
library(tidyverse)
library(reshape2)

#Load Data
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

land = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

date = land %>% 
  group_by(index, year) %>% 
  summarise(value, skewness(value))

#Select indices
ndvi = land %>%
  filter(index == "ndvi")

evi = land %>%
  filter(index == "evi2")

ndii = land %>%
  filter(index == "ndii")

vig = land %>%
  filter(index == "vig")

#Skewness and Kurtosis calculation
sks = skewness(ndvi$value); sks
curt = kurtosis(ndvi$value); curt

#Plot results
ggplot(ndvi, aes(value))+
  geom_density(aes(fill = treat), alpha = 0.35)


ggplot(ndvi, aes(x = value, y = year, fill = treat)) +
  geom_density_ridges(alpha = 0.35) +
  #facet_wrap(~index, scales="free") +
  theme_minimal()







