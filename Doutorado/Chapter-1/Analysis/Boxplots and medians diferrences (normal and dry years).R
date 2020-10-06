#Density of Hyperion Index all data

#Eduardo Q Marques 11-03-2020

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)
library(ggridges)

#Data ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')
df$year = as.character(df$year)

eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange, Red and Blue)

#Structural
struc = df %>% 
  filter(index %in% c('evi','ndvi','vari','vig'))


a = ggplot(struc, aes(y = value, x = year, fill=parcela)) +
  #geom_density_ridges(alpha = 0.35) +
  geom_boxplot()+
  facet_wrap(~index, scales="free") +
  theme_minimal()

struc = ggpar(a, palette = eqm)
struc

#Biochemistry
bioc = df %>% 
  filter(index %in% c('nirv','lwvi2','msi','ndii','ndwi','pssr','psri','sipi','wbi'))


a = ggplot(bioc, aes(y = value, x = year, fill=parcela)) +
  #geom_density_ridges(alpha = 0.35) +
  geom_boxplot()+
  facet_wrap(~index, scales="free") +
  theme_minimal()

bioc = ggpar(a, palette = eqm)
bioc

#Physiologic
phy = df %>% 
  filter(index %in% c('pri','rendvi'))

a = ggplot(phy, aes(y = value, x = year, fill=parcela)) +
  #geom_density_ridges(alpha = 0.35) +
  geom_boxplot()+
  facet_wrap(~index, scales="free") +
  theme_minimal()

phy = ggpar(a, palette = eqm)
phy

#Result
#Controle
res = df %>% 
  filter(index %in% c('pssr','rendvi')) %>% 
  filter(parcela == "control")

ggplot(res, aes(y = value, x = year)) +
  #geom_density_ridges(alpha = 0.75, fill = "#00AFBB") +
  geom_boxplot(fill = "#00AFBB")+
  facet_wrap(~index, scales="free") +
  theme_minimal()

#Burned
burn = df %>% 
  filter(index %in% c('pssr')) %>% 
  filter(parcela != "control")

a = ggplot(burn, aes(y = value, x = year, fill = parcela)) +
  #geom_density_ridges(alpha = 0.35) +
  geom_boxplot()+
  facet_wrap(~index, scales="free") +
  theme_minimal()

burn = ggpar(a, palette = eqm)
burn

#Others
burn2 = df %>% 
  filter(index %in% c('nirv','lwvi2','rendvi','vari')) %>% 
  filter(parcela != "control")

a = ggplot(burn2, aes(y = value, x = year, fill = parcela)) +
  #geom_density_ridges(alpha = 0.35) +
  geom_boxplot()+
  facet_wrap(~index, scales="free") +
  theme_minimal()

burn2 = ggpar(a, palette = eqm)
burn2



