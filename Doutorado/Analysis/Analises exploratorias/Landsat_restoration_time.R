#Landsat Restoration Time
#By: Eduardo Q Marques 03-03-2020

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)
library(ggridges)

#Load data ==========================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

land = read.csv("Landast_indexs_all by plot.csv", sep = ',')
land$data = as.character(land$data)

#biomass = read.csv("Biomass_tang.csv", sep = ",", header = TRUE)
#biomass = read.csv("Biomass_full_tang.csv", sep = ",", header = TRUE)

#lai = read.csv("LAI_tang.csv", sep = ",", header = TRUE)
#lai = read.csv("LAI_full_tang.csv", sep = ",", header = TRUE)

#litt  = read.csv("Liteira_tang.csv", sep = ",", header = TRUE)
#litt  = read.csv("Liteira_full_tang.csv", sep = ",", header = TRUE)

#fuel = read.csv("Fuel_tang.csv", sep = ",", header = TRUE)
#fuel = read.csv("Fuel_full_tang.csv", sep = ",", header = TRUE)

#fire = read.csv("Fire.csv", sep = ",", header = TRUE) #Make the fire intensity!

#Boxplots ======================================
#Restoration time
eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange and Blue)

land2 = melt(land)

land3 = land2 %>% 
  filter(data > 2012) %>% 
  filter(variable %in% c('ndvi','evi','vig'))


a = ggplot(land3, aes(data,value, fill=parcela))+ 
  geom_boxplot(outlier.alpha = 0.3)+
  #geom_violin()+
  #facet_wrap(~variable, scales="free") +
  facet_grid(variable ~ ., scales="free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

a = ggpar(a, palette = eqm)


land4 = land2 %>% 
  filter(data > 2012) %>% 
  filter(variable %in% c('ndwi','ndii','nbri'))

b = ggplot(land4, aes(data,value, fill=parcela))+ 
  geom_boxplot(outlier.alpha = 0.3)+
  facet_grid(variable ~ ., scales="free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

b = ggpar(b, palette = eqm)

land_time = ggarrange(a, b, common.legend = TRUE, legend="bottom", ncol = 2, nrow = 1)

land_time
