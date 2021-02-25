#Cap-1 Correlation Matrix on filtered exploration
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

#Boxplots ======================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_median by plot.csv", sep = ',')
hy$data = as.character(hy$data)
#Exclude MSI, NBRI, SIPI, PSRI, VARI, ARI
hy = hy[,c(-2,-5,-6,-11,-14,-15)]

#hy = read.csv("Hyperion_indexs_all by plot.csv", sep = ',')
#hy = hy[,c(-5, -9)]
#hy$data = as.character(hy$data)
#Exclude MSI, NBRI, SIPI, PSRI, VARI, ARI
#hy2 = hy[,c(-3, -4, -9, -12, -13, -16)]

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

#Vegetation Index Correlation =============================================================
ggcorr(df, label = TRUE)

#Making data
hy2 = melt(hy)
df2 = hy2

df2 = full_join(hy2, lai, by="id")
df2 = full_join(df2, litt, by="id")
df2 = full_join(df2, biomass, by="id")
df2 = full_join(df2, fuel, by="id")

colnames(df2) = c('id', 'index', 'value', 'lai', 'litter', 'biomass', 'fuel')

df2 = df2 %>% 
  #na.omit() %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')


eqm = c("#FC4E07","#00AFBB") #Pallete colors(Orange and Blue)

#Correlation =============================================================
df2 = df2 %>% 
  filter(parcela != 'controle')

a = ggplot(df2, aes(x=value, y=lai, color=parcela))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=F)+ 
  facet_wrap(~index, scales="free") +
  stat_cor(show.legend = F)+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
  theme(legend.position="bottom")

a = ggpar(a, palette = eqm)
a


b = ggplot(df2, aes(x=value, y=litter, color=parcela))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=F)+ 
  facet_wrap(~index, scales="free") +
  stat_cor(show.legend = F)+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
  theme(legend.position="bottom")

b = ggpar(b, palette = eqm)
b
#corr = ggarrange(a + rremove("xlab"),
#                   b + rremove("xlab"),
#                   common.legend = TRUE,
#                   legend="bottom",
#                   ncol = 1, nrow = 2)

#corr



