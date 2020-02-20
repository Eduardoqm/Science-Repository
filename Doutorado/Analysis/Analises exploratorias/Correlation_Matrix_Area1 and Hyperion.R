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

ggcorr(struc, label = TRUE)
ggpairs(struc, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('evi','ndvi','nbri','vari','vig','lai','litter'))


bioc = df %>% 
  select('dist','ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel')

ggcorr(bioc, label = TRUE)
ggpairs(bioc, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','lai','litter'))


phy = df %>% 
  select('dist','pri','rendvi','biomass','lai','litter','fuel')

ggcorr(phy, label = TRUE)
ggpairs(phy, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('pri','rendvi','lai','litter'))






bioc2 = df %>% 
  select('data','parcela','dist','ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi')

bioc2 = melt(bioc2)
colnames(bioc2) = c('data','parcela','dist','variable','index')

bioc3 = df %>% 
  select('data','parcela','dist','biomass','lai','litter','fuel')

bioc3 = melt(bioc3)
colnames(bioc3) = c('data','parcela','dist','variable2','plot_data')

bioc4 = full_join(bioc2, bioc3)
bioc4 = bioc4 %>% 
  na.omit()

library(viridis)

ggplot(bioc4, aes(x=plot_data, y=index) ) +
  geom_point(aes(col=variable))+
  #geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()




ggplot(phy, aes(x=rendvi, y=litter)) + 
  geom_point(aes(col=dist)) + 
  geom_smooth(method="loess", se=F) + 
  labs(y="Index", 
       x="Litterfall")

a = lm(phy$litter ~ phy$rendvi)

ggplot(phy) + 
  geom_point(aes(x=rendvi, y=litter, col = dist)) + 
  geom_point(aes(x=pri, y=litter, col = dist)) + 
  geom_point(aes(x=rendvi, y=lai, col = dist)) + 
  geom_point(aes(x=pri, y=lai, col = dist)) + 
  #geom_smooth(aes(x=rendvi, y=litter), method="loess", se=T) + 
  labs(y="Plot data", 
       x="Index")



phy = df %>% 
  select('dist','pri','rendvi','biomass','lai','litter','fuel')


ggpairs(phy, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('pri','rendvi','biomass','lai','litter','fuel'))

ggpairs(phy, aes(color = dist), axisLabels = "none", density =  ,
        columns = c('pri','rendvi','biomass','lai','litter','fuel'))




ggduo(phy, aes(color = dist), types = list(continuous = "smooth"), axisLabels = "none")

ggduo(phy, aes(color = dist), axisLabels = "none")

ggcorr(phy)









