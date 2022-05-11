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

colnames(df)[9] = c("Tratamento")

eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange, Red and Blue)
#eqm = c("orange", "red", "blue")

a = ggplot(df, aes(x = value, y = year, fill=Tratamento)) +
  geom_density_ridges(alpha = 0.30) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  theme(legend.position = c(0.5, 0.1))+
  labs(x = "", y = ""); a


ggsave(filename = "all_density.png", plot = a,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Densidade dos indices",
       width = 30, height = 20, units =  "cm", dpi = 300)

#Structural
struc = df %>% 
  filter(index %in% c('EVI','NDVI','VARI','VIG'))# %>% 
  #group_by(index, year) %>% 
  #mutate(value = value - mean(value, na.rm = T))


a = ggplot(struc, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.30) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  labs(x = "", y = "")

a
#ggsave(filename = "Structural.png", plot = a,
#       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Densidade dos indices", 
#       width = 30, height = 15, units =  "cm", dpi = 300)

#Biochemistry
bioc = df %>% 
  filter(index %in% c('NIRv','LWVI2','MSI','NDII','NDWI','PSSR','PSRI','SIPI','WBI'))# %>% 
  #group_by(index, year) %>% 
  #mutate(value = value - mean(value, na.rm = T))

a = ggplot(bioc, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  labs(x = "", y = "")

a
#ggsave(filename = "Biochemistry.png", plot = a,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Densidade dos indices", 
  #     width = 30, height = 15, units =  "cm", dpi = 300)

#Physiologic and Fire
phy_fire = df %>% 
  filter(index %in% c('PRI','RENDVI', 'NBR','NBR2'))# %>% 
  #group_by(index, year) %>% 
  #mutate(value = value - mean(value, na.rm = T))

a = ggplot(phy_fire, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  labs(x = "", y = "")

a
#ggsave(filename = "Physiologic_Fire.png", plot = a,
#       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Densidade dos indices", 
#       width = 30, height = 15, units =  "cm", dpi = 300)


