#Density of Hyperion Index all data (Centralized)

#Eduardo Q Marques 20-07-2021

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

df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

df$year = as.character(df$year)
df$index = as.character(df$index)
df$treat = as.character(df$treat)

#Change names format
df$index[df$index == "evi2"] <- c("EVI")
df$index[df$index == "ndvi"] <- c("NDVI")
df$index[df$index == "ndii"] <- c("NDII")
df$index[df$index == "vig"] <- c("VIG")
df$index[df$index == "vari"] <- c("VARI")
df$index[df$index == "nirv"] <- c("NIRv")
df$index[df$index == "lwvi2"] <- c("LWVI2")
df$index[df$index == "msi"] <- c("MSI")
df$index[df$index == "ndwi"] <- c("NDWI")
df$index[df$index == "pssr"] <- c("PSSR")
df$index[df$index == "psri"] <- c("PSRI")
df$index[df$index == "sipi"] <- c("SIPI")
df$index[df$index == "wbi"] <- c("WBI")
df$index[df$index == "pri"] <- c("PRI")
df$index[df$index == "rendvi"] <- c("RENDVI")
df$index[df$index == "nbr"] <- c("NBR")
df$index[df$index == "nbr2"] <- c("NBR2")



df$treat[df$treat == "control"] <- c("Controle")
df$treat[df$treat == "b3yr"] <- c("B3yr")
df$treat[df$treat == "b1yr"] <- c("B1yr")

colnames(df)[9] = c("Parcela")

eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange, Red and Blue)
#eqm = c("orange", "red", "blue")

#Structural
struc = df %>% 
  filter(index %in% c('EVI','NDVI','VARI','VIG','biomass','lai','litter','fuel'))


struc2 = df %>% 
  filter(index %in% c('EVI','NDVI','VARI','VIG','biomass','lai','litter','fuel')) %>% 
  group_by(index, year) %>% 
  mutate(value = value - mean(value, na.rm = T))


ggplot(struc2, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.30) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  labs(x = "", y = "")

#Biochemistry
bioc = df %>% 
  filter(index %in% c('NIRv','LWVI2','MSI','NDII','NDWI','PSSR','PSRI','SIPI','WBI','biomass','lai','litter','fuel')) %>% 
  group_by(index, year) %>% 
  mutate(value = value - mean(value, na.rm = T))

ggplot(bioc, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  labs(x = "", y = "")

#Physiologic
phy = df %>% 
  filter(index %in% c('PRI','RENDVI','biomass','lai','litter','fuel')) %>% 
  group_by(index, year) %>% 
  mutate(value = value - mean(value, na.rm = T))

ggplot(phy, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  labs(x = "", y = "")

#Fire Indices
fire = df %>% 
  filter(index %in% c('NBR','NBR2','biomass','lai','litter','fuel'))

ggplot(fire, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  labs(x = "", y = "")

#Result
#Controle
res = df %>% 
  filter(index %in% c('PSSR', 'Red-Edge NDVI')) %>% 
  filter(Parcela != "controle")

ggplot(res, aes(x = value, y = year)) +
  geom_density_ridges(alpha = 0.75, fill = "#00AFBB") +
  facet_wrap(~index, scales="free") +
  theme_minimal()

#Burned
burn = df %>% 
  filter(index %in% c('PSSR')) %>% 
  filter(Parcela != "controle")

ggplot(burn, aes(x = value, y = year, fill = Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Others
burn2 = df %>% 
  filter(index %in% c('NIRv','LWVI2','Red-Edge NDVI','VARI')) %>% 
  filter(Parcela != "controle")

ggplot(burn2, aes(x = value, y = year, fill = Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))
