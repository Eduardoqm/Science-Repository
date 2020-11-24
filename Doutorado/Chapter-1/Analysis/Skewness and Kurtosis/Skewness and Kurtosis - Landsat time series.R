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

#Skewness and Kurtosis calculation
land_sks = land %>% 
  group_by(index, treat, year) %>% 
  summarise(value = skewness(value))

land_curt = land %>% 
  group_by(index, treat, year) %>% 
  summarise(value = kurtosis(value))

#Select indices (Skewness)
ndvi = land_sks %>%
  filter(index == "ndvi")

evi = land_sks %>%
  filter(index == "evi2")

ndii = land_sks %>%
  filter(index == "ndii")

vig = land_sks %>%
  filter(index == "vig")

#Plot results
ggplot(ndvi, aes(x=as.factor(year), y=value))+
  geom_line(aes(group = treat, col = treat), size = 1)+
  geom_point(aes(col = treat), size = 3)+
  theme_bw()+
  ggtitle("Skewness (NDVI-Landsat)")+
  theme(axis.text.x = element_text(angle=90))


#Select indices (Kurtosis)
ndvi = land_curt %>%
  filter(index == "ndvi")

evi = land_curt %>%
  filter(index == "evi2")

ndii = land_curt %>%
  filter(index == "ndii")

vig = land_curt %>%
  filter(index == "vig")

#Plot results
ggplot(ndvi, aes(x=as.factor(year), y=value))+
  geom_line(aes(group = treat, col = treat), size = 1)+
  geom_point(aes(col = treat), size = 3)+
  theme_bw()+
  ggtitle("Kurtosis (NDVI-Landsat)")+
  theme(axis.text.x = element_text(angle=90))





