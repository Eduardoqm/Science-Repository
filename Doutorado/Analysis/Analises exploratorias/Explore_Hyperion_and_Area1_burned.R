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
library(ggridges)

#Load data ==========================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

#hy = read.csv("Hyperion_indexs_median by plot.csv", sep = ",", header = TRUE)
#hy = hy[,c(-12)] #PSSR have so high values in comparison with other indexs
#hy = hy[,c(-9)] #NDWI have so high values in comparison with other indexs

hy = read.csv("Hyperion_indexs_all by plot.csv", sep = ',')
hy$data = as.character(hy$data)

#biomass = read.csv("Biomass_tang.csv", sep = ",", header = TRUE)
biomass = read.csv("Biomass_full_tang.csv", sep = ",", header = TRUE)

#lai = read.csv("LAI_tang.csv", sep = ",", header = TRUE)
lai = read.csv("LAI_full_tang.csv", sep = ",", header = TRUE)

#litt  = read.csv("Liteira_tang.csv", sep = ",", header = TRUE)
litt  = read.csv("Liteira_full_tang.csv", sep = ",", header = TRUE)

#fuel = read.csv("Fuel_tang.csv", sep = ",", header = TRUE)
fuel = read.csv("Fuel_full_tang.csv", sep = ",", header = TRUE)

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

lai = as.data.frame(lai)
lai = lai %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

litt = as.data.frame(litt)
litt = litt %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')
colnames(litt) = c("id", "litter")

fuel = as.data.frame(fuel)
fuel = fuel %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')
colnames(fuel) = c("id", "fuel")


df = hy
#df = full_join(df, biomass, by="id")
#df = full_join(df, lai, by="id")
#df = full_join(df, litt, by="id")
#df = full_join(df, fuel, by="id")

df = df %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')


#Boxplots ======================================
#Control x B1yr
df2 = melt(df)

eqm = c("#F9A602","#CF0E0E","#00AFBB") #Pallete colors(Orange and Blue)

#Structural
struc = df2 %>% 
  filter(variable %in% c('evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel')) #%>% 
  #filter(parcela != "b1yr") %>% 
  #filter(dist == "borda")


a = ggplot(struc, aes(data,value, fill=parcela))+ 
  geom_boxplot(outlier.alpha = 0.3)+
  #geom_violin()+
  facet_wrap(~variable, scales="free") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

struc_control = ggpar(a, palette = eqm)
struc_control

#Biochemistry
bioc = df2 %>% 
  filter(variable %in% c('ari','lwvi2','msi','ndii','ndwi','pssr','psri','sipi','wbi','biomass','lai','litter','fuel')) #%>% 
  #filter(parcela != "b1yr") %>% 
  #filter(dist == "borda")


a = ggplot(bioc, aes(data,value, fill=parcela))+ 
  geom_boxplot(outlier.alpha = 0.3)+
  #geom_violin()+
  facet_wrap(~variable, scales="free") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

bioc_control = ggpar(a, palette = eqm)
bioc_control

#Physiologic
phy = df2 %>% 
  filter(variable %in% c('pri','rendvi','biomass','lai','litter','fuel')) #%>% 
  #filter(parcela != "b1yr") %>% 
  #filter(dist == "borda")


a = ggplot(phy, aes(data,value, fill=parcela))+ 
  geom_boxplot(outlier.alpha = 0.3)+
  #geom_violin()+
  facet_grid(variable ~ ., scales="free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

phy_control = ggpar(a, palette = eqm)
phy_control


#ggplot(df2, aes(x = value, y = variable, fill=dist)) +
#  geom_density_ridges() +
#  labs(x="Value (% Relative difference)",y=" ")+
#  theme_minimal()