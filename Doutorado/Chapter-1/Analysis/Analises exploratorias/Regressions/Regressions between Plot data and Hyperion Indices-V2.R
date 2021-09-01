#Regression between Plot data and Hyperion Indices - V2

#Eduardo Q Marques 01-09-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggvis)
library(mgcv)
library(visreg)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Hyperion =========================================================
#Data ------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")
df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')
lai = read.csv("LAI_full_tang.csv", sep = ',')
litt = read.csv("Liteira_full_tang.csv", sep = ',')
bmas = read.csv("Biomass_full_tang.csv", sep = ',')

#Modify and filter date to match ---------------------------------
df = df %>% 
  na.omit() %>% 
  unite("id", c("treat", "year"), sep = "_")

df2 = df[,c(5,6,7)]
df2 = df2 %>%
  group_by(id, index) %>% 
  summarise(value = mean(value))
df2 = df2 %>%
  #filter(index %in% c("pssr", "psri", "evi2", "rendvi", "ndwi", "msi"))
  filter(index %in% c("ndvi", "msi", "ndii", "vig", "psri"))


df2$index = as.character(df2$index)
df2$index[df2$index == "ndvi"] <- c("NDVI")
df2$index[df2$index == "msi"] <- c("MSI")
df2$index[df2$index == "ndii"] <- c("NDII")
df2$index[df2$index == "vig"] <- c("VIG")
df2$index[df2$index == "psri"] <- c("PSRI")

lai = lai %>% 
  unite("id", c("parcela", "year"), sep = "_")

lai2 = lai[,c(3,4)]
lai2 = lai2 %>% 
  group_by(id) %>% 
  summarise(lai = mean(lai))


litt = litt %>% 
  unite("id", c("parcela", "year"), sep = "_")

litt2 = litt[,c(1,7)]
litt2 = litt2 %>% 
  group_by(id) %>% 
  summarise(lit_ton_hec = mean(lit_ton_hec))


bmas = bmas %>% 
  unite("id", c("parcela", "data"), sep = "_")

bmas2 = bmas[,c(1,4)]
bmas2 = bmas2 %>% 
  group_by(id) %>% 
  summarise(biomass = sum(biomass))

#Join Data ---------------------------------------------------------
df_lai = full_join(df2, lai2, by = "id")
df_lai = na.omit(df_lai)

df_litt = full_join(df2, litt2, by = "id")
df_litt = na.omit(df_litt)

df_bmas = full_join(df2, bmas2, by = "id")
df_bmas = na.omit(df_bmas)

#Plot correlation --------------------------------------------------
lai_cor = function(x){
  z = df_lai %>% 
    filter(index == x) 
    cor(z$value, z$lai)
}

lai_cor("PSRI")







