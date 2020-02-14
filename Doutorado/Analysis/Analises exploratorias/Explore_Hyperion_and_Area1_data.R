#Analysis Cap-1 Hyperion Indexs on Fire
#By: Eduardo Q Marques 14-02-2020
#Obs: looking Analysis Cap1.R

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)

#Load data
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_median by plot.csv", sep = ",", header = TRUE)
biomass = read.csv("Biomass_tang.csv", sep = ",", header = TRUE)
lai = read.csv("LAI_tang.csv", sep = ",", header = TRUE)
litt  = read.csv("Liteira_tang.csv", sep = ",", header = TRUE)
#fuel = read.csv("Combustivel_Brown_Tang.csv", sep = ",", header = TRUE)
#fire = read.csv("Fire.csv", sep = ",", header = TRUE) #Make the fire intensity!


#Data organization
#From Hyperion we have 2004:2012, so let's try use other datas in this range
#Biomass
biomass = biomass %>% 
  filter(data <=2012)

#LAI
lai = lai %>% 
  filter(data <=2012)

#Litterfall
litt = litt %>% 
  filter(data <=2012)

#Join Data
hy = hy %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

biomass = as.data.frame(biomass)#To work
biomass = biomass %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

lai = lai %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

litt = litt %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')


df = hy
#df = full_join(df, biomass, by="id")
df = full_join(df, lai, by="id")
df = full_join(df, fuel, by="id")
df = full_join(df, litt, by="id")

df = df %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')

#write.table(df, "Area1_data_edge_core.csv", sep = ",")
#Correlation Matrix ======================
ggpairs(df)

#GLM and Dredge

