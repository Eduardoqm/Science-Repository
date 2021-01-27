#Correlation between Plot data and Satellites Indices

#Eduardo Q Marques 27-01-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(mgcv)
library(visreg)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Landsat =========================================================
#Data ------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')
lai = read.csv("LAI_full_tang.csv", sep = ',')
litt = read.csv("Liteira_full_tang.csv", sep = ',')

#Modify and filter date to match ---------------------------------
df$year = substr(df$year, 1,4)
df = df %>% 
  filter(year == c(2005:2017)) %>% 
  unite("id", c("treat", "year"), sep = "_")
df2 = df[,c(4,5,6)]

lai = lai %>% 
  filter(year != 2012) %>% 
  unite("id", c("parcela", "year"), sep = "_")
lai2 = lai[,c(3,4)]

litt = litt %>% 
  filter(year == c(2005:2017)) %>% 
  filter(year != 2012) %>% 
  unite("id", c("parcela", "year"), sep = "_")
litt2 = litt[,c(1,5)]

#Join Data ---------------------------------------------------------
df3 = full_join(df2, lai2, by = "id")
df4 = full_join(df3, litt2, by = "id")

#Plot correlation --------------------------------------------------
ggplot(df4, aes(x=value, y=lai))+
  geom_point()+
  facet_wrap(~index)






#-----------------------------------





























df$year = as.numeric(df$year)
df$index = as.character(df$index)
df$treat = as.character(df$treat)

#Modify elements of dataframe
df$index[df$index == "evi2"] <- c("EVI")
df$index[df$index == "ndvi"] <- c("NDVI")
df$index[df$index == "ndii"] <- c("NDII")
df$index[df$index == "vig"] <- c("VIG")

df$treat[df$treat == "control"] <- c("Controle")
df$treat[df$treat == "b3yr"] <- c("B3yr")
df$treat[df$treat == "b1yr"] <- c("B1yr")










eqm = c("orange", "red", "blue") #My color palette










