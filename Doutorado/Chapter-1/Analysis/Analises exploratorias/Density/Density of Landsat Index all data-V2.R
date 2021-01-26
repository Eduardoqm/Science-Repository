#Density of Landsat Index all data

#Eduardo Q Marques 25-11-2020
#Updated in 26-01-2021

#Include the new dataframe transformation for Landsat

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

#Data ============================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify Data =====================================================
df$year = substr(df$year, 1,4)
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

eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange, Red and Blue)

colnames(df)[8] = c("Parcela")

#Filter to get regeneration
df2 = df
#df2 = df %>% 
#  filter(year > 2000)

evi = df2 %>%
filter(index == 'EVI')

ndvi = df2 %>%
  filter(index == 'NDVI')

vig = df2 %>%
  filter(index == 'VIG')

ndii = df2 %>%
  filter(index == 'NDII')


land1 = ggplot(evi, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  labs(x = "")+
  theme(text = element_text(family = "Times New Roman", size = 14))

land1

land2 = ggplot(ndvi, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  labs(x = "", y = "")+
  theme(text = element_text(family = "Times New Roman", size = 14))

land2

land3 = ggplot(vig, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  labs(x = "", y = "")+
  theme(text = element_text(family = "Times New Roman", size = 14))

land3

land4 = ggplot(ndii, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()+
  scale_fill_manual(values = eqm)+
  labs(x = "", y = "")+
  theme(text = element_text(family = "Times New Roman", size = 14))

land4


land = ggarrange(land1 + rremove("xlab"),
                 land2 + rremove("xlab") + rremove("ylab"),
                 land3 + rremove("xlab") + rremove("ylab"),
                 land4 + rremove("xlab") + rremove("ylab"),
                 common.legend = TRUE,
                 legend="bottom",
                 ncol = 4, nrow = 1)


land


#Result
#NDVI
res1 = df2 %>%
  filter(index == 'ndvi') %>% 
  filter(Parcela != 'controle')

a = ggplot(res1, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

res1 = ggpar(a, palette = eqm)
res1

#NDII
res2 = df2 %>%
  filter(index == 'ndii') %>% 
  filter(Parcela != 'controle')

a = ggplot(res2, aes(x = value, y = year, fill=Parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

res2 = ggpar(a, palette = eqm)
res2






