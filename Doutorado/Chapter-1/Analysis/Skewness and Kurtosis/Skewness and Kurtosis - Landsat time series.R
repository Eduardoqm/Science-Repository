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
land$year = as.character(land$year)

land$year = substr(land$year, 1,4)

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
eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange, Red and Blue)
plts = function(z, w){
  ggplot(z, aes(x=year, y=value))+
    geom_line(aes(group = treat, col = treat), size = 1)+
    geom_point(aes(col = treat), size = 3)+
    scale_color_manual(values = eqm)+
    theme_bw()+
    ggtitle(paste("Skewness", w))+
    theme(axis.text.x = element_text(angle=90))
}

plts(ndvi, "NDVI")
plts(evi, "EVI")
plts(ndii, "NDII")
plts(vig, "VIG")


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
pltk = function(z, w){
  ggplot(z, aes(x=year, y=value))+
    geom_line(aes(group = treat, col = treat), size = 1)+
    geom_point(aes(col = treat), size = 3)+
    scale_color_manual(values = eqm)+
    theme_bw()+
    ggtitle(paste("Kurtosis", w))+
    theme(axis.text.x = element_text(angle=90))
}

pltk(ndvi, "NDVI")
pltk(evi, "EVI")
pltk(ndii, "NDII")
pltk(vig, "VIG")

#Save dataframe of tests
colnames(land_sks)[4] = "skewness"
land_sks = land_sks %>% 
  unite("id", index, treat, year, sep = "_")
  
colnames(land_curt)[4] = "kurtosis"
land_curt = land_curt %>% 
  unite("id", index, treat, year, sep = "_")


df = full_join(land_sks, land_curt, by = "id")
df = df %>% 
  separate(id, c("index", "treat", "year"), sep = "_")

#write.table(df, file = "skewness_kurtosis_landsat.csv", sep = ",", row.names = F)















