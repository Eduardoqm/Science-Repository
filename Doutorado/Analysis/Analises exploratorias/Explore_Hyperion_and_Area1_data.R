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
hyper = hyper[,c(-9)] #NDWI have so high values in comparison with other indexs

biomass = read.csv("Biomass_tang.csv", sep = ",", header = TRUE)

lai = read.csv("LAI_tang.csv", sep = ",", header = TRUE)

litt  = read.csv("Liteira_tang.csv", sep = ",", header = TRUE)

#fuel = read.csv("Combustivel_Brown_Tang.csv", sep = ",", header = TRUE)
#fire = read.csv("Fire.csv", sep = ",", header = TRUE) #Make the fire intensity!


#Data integration =======================================================================
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
df = full_join(df, biomass, by="id")
df = full_join(df, lai, by="id")
df = full_join(df, litt, by="id")

df = df %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')

#write.table(df, "Integration_Area1_data_edge_core.csv", sep = ",")
#Correlation Matrix ======================================================================
ggpairs(df)

#Data organization =======================================================================
#Separate by edge and core and tratament
df_edge_crt = df %>%
  filter(dist == "borda", parcela == "controle")

df_edge_b3yr = df %>%
  filter(dist == "borda", parcela == "b3yr")

df_edge_b1yr = df %>%
  filter(dist == "borda", parcela == "b1yr")

df_core_crt = df %>%
  filter(dist == "nucleo", parcela == "controle")

df_core_b3yr = df %>%
  filter(dist == "nucleo", parcela == "b3yr")

df_core_b1yr = df %>%
  filter(dist == "nucleo", parcela == "b1yr")

#Calculate difference in relation to control
#df Edge
df_edge_crt$data = as.character(df_edge_crt$data)
df_edge_crt = melt(df_edge_crt)

df_edge_b3yr$data = as.character(df_edge_b3yr$data)
df_edge_b3yr = melt(df_edge_b3yr)

df_edge_b1yr$data = as.character(df_edge_b1yr$data)
df_edge_b1yr = melt(df_edge_b1yr)


df_diff_edge = df_edge_crt[,c(1,2,3,4)]
df_diff_edge$b3yr = ((df_edge_b3yr$value-df_edge_crt$value)*100)/df_edge_crt$value
df_diff_edge$b1yr = ((df_edge_b1yr$value-df_edge_crt$value)*100)/df_edge_crt$value

df_diff_edge = df_diff_edge[,c(-1)]

#df Core
df_core_crt$data = as.character(df_core_crt$data)
df_core_crt = melt(df_core_crt)

df_core_b3yr$data = as.character(df_core_b3yr$data)
df_core_b3yr = melt(df_core_b3yr)

df_core_b1yr$data = as.character(df_core_b1yr$data)
df_core_b1yr = melt(df_core_b1yr)


df_diff_core = df_core_crt[,c(1,2,3,4)]
df_diff_core$b3yr = ((df_core_b3yr$value-df_core_crt$value)*100)/df_core_crt$value
df_diff_core$b1yr = ((df_core_b1yr$value-df_core_crt$value)*100)/df_core_crt$value

df_diff_core = df_diff_core[,c(-1)]






















