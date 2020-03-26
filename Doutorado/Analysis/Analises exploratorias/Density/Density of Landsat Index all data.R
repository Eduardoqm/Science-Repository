#Density of Landsat Index all data

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
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

df = read.csv("Landast_indexs_all by plot.csv", sep = ',')
df$data = as.character(df$data)

df2 = melt(df)
colnames(df2) = c("year", "parcela", "dist","index","value")

#Filter to get regeneration
df2 = df2 %>% 
  filter(year > 2012)

evi = df2 %>%
filter(index == 'evi')

ndvi = df2 %>%
  filter(index == 'ndvi')

vig = df2 %>%
  filter(index == 'vig')

ndii = df2 %>%
  filter(index == 'ndii')

eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange, Red and Blue)


a = ggplot(evi, aes(x = value, y = year, fill=parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

land1 = ggpar(a, palette = eqm)
land1

a = ggplot(ndvi, aes(x = value, y = year, fill=parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

land2 = ggpar(a, palette = eqm)
land2

a = ggplot(vig, aes(x = value, y = year, fill=parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

land3 = ggpar(a, palette = eqm)
land3

a = ggplot(ndii, aes(x = value, y = year, fill=parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

land4 = ggpar(a, palette = eqm)
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
  filter(parcela != 'controle')

a = ggplot(res1, aes(x = value, y = year, fill=parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

res1 = ggpar(a, palette = eqm)
res1

#NDII
res2 = df2 %>%
  filter(index == 'ndii') %>% 
  filter(parcela != 'controle')

a = ggplot(res2, aes(x = value, y = year, fill=parcela)) +
  geom_density_ridges(alpha = 0.35) +
  facet_wrap(~index, scales="free") +
  theme_minimal()

res2 = ggpar(a, palette = eqm)
res2
