#Density of Hyperion Index all data

#Eduardo Q Marques 26-01-2021

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')
df$year = as.character(df$year)

#eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange, Red and Blue)
eqm = c("orange", "red", "blue")

#Structural
struc = df %>% 
  filter(index %in% c('evi','ndvi','vari','vig','biomass','lai','litter','fuel'))


ggplot(struc, aes(x = value, y = year, fill=parcela)) +
  geom_density_ridges(alpha = 0.30) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Biochemistry
bioc = df %>% 
  filter(index %in% c('nirv','lwvi2','msi','ndii','ndwi','pssr','psri','sipi','wbi','biomass','lai','litter','fuel'))


a = ggplot(bioc, aes(x = value, y = year, fill=parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

bioc = ggpar(a, palette = eqm)
bioc

#Physiologic
phy = df %>% 
  filter(index %in% c('pri','rendvi','biomass','lai','litter','fuel'))

a = ggplot(phy, aes(x = value, y = year, fill=parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

phy = ggpar(a, palette = eqm)
phy

#Result
#Controle
res = df %>% 
  filter(index %in% c('pssr','rendvi')) %>% 
  filter(parcela == "control")

ggplot(res, aes(x = value, y = year)) +
  geom_density_ridges(alpha = 0.75, fill = "#00AFBB") +
  facet_wrap(~index, scales="free") +
  theme_minimal()

#Burned
burn = df %>% 
  filter(index %in% c('pssr')) %>% 
  filter(parcela != "control")

a = ggplot(burn, aes(x = value, y = year, fill = parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

burn = ggpar(a, palette = eqm)
burn

#Others
burn2 = df %>% 
  filter(index %in% c('nirv','lwvi2','rendvi','vari')) %>% 
  filter(parcela != "control")

a = ggplot(burn2, aes(x = value, y = year, fill = parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

burn2 = ggpar(a, palette = eqm)
burn2



