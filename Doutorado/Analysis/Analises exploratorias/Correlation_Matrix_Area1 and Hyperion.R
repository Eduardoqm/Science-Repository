#Correlation Matrix Cap-1 Hyperion Indexs on Fire
#By: Eduardo Q Marques 20-02-2020
#Obs: looking Analysis Cap1.R

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)
library(plotly)

#Load data ==========================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_median by plot.csv", sep = ",", header = TRUE)
hy = hy[,c(-9)] #NDWI have so high values in comparison with other indexs

biomass = read.csv("Biomass_tang.csv", sep = ",", header = TRUE)

lai = read.csv("LAI_tang.csv", sep = ",", header = TRUE)

litt  = read.csv("Liteira_tang.csv", sep = ",", header = TRUE)

fuel = read.csv("Fuel_tang.csv", sep = ",", header = TRUE)

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
colnames(biomass) = c("id", "biomass")

lai = lai %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

litt = litt %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')
colnames(litt) = c("id", "litter")

fuel = fuel %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')
colnames(fuel) = c("id", "fuel")


df = hy
df = full_join(df, biomass, by="id")
df = full_join(df, lai, by="id")
df = full_join(df, litt, by="id")
df = full_join(df, fuel, by="id")

df = df %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')


#Correlations =============================================================================
struc = df %>% 
  select('dist','evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel')

ggcorr(struc)
ggpairs(struc, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('evi','ndvi','nbri','vari','vig','lai','litter'))


bioc = df %>% 
  select('dist','ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel')

ggcorr(bioc)
ggpairs(bioc, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','lai','litter'))


phy = df %>% 
  select('dist','pri','rendvi','biomass','lai','litter','fuel')

ggcorr(phy)
ggpairs(phy, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('pri','rendvi','lai','litter'))












phy = df %>% 
  select('dist','pri','rendvi','biomass','lai','litter','fuel')


ggpairs(phy, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('pri','rendvi','biomass','lai','litter','fuel'))

ggpairs(phy, aes(color = dist), axisLabels = "none", density =  ,
        columns = c('pri','rendvi','biomass','lai','litter','fuel'))




ggduo(phy, aes(color = dist), types = list(continuous = "smooth"), axisLabels = "none")

ggduo(phy, aes(color = dist), axisLabels = "none")

ggcorr(phy)









