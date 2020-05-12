#Correlation Matrix Hyperion and full plot data
#By: Eduardo Q Marques 11-05-2020
#Obs: looking Analysis Cap1.R

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)

#Load data ==========================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")


hy = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')
#hy = hy[,c(-5, -9)] #NDWI have so high values in comparison with other indexs
hy$year = as.character(hy$year)

#biomass = read.csv("Biomass_full_tang.csv", sep = ",", header = TRUE)
biomass = read.csv("Biomass_tang.csv", sep = ",", header = TRUE)

#lai = read.csv("LAI_full_tang.csv", sep = ",", header = TRUE)
lai = read.csv("LAI_tang.csv", sep = ",", header = TRUE)

#litt  = read.csv("Liteira_full_tang.csv", sep = ",", header = TRUE)
litt  = read.csv("Liteira_tang.csv", sep = ",", header = TRUE)

#fuel = read.csv("Fuel_full_tang.csv", sep = ",", header = TRUE)
fuel = read.csv("Fuel_tang.csv", sep = ",", header = TRUE)

#Data organization =======================================================================
#From Hyperion we have 2004:2012, so let's try use other datas in this range
#Biomass
biomass = biomass %>% 
  filter(data <=2012)

#LAI
lai$data = as.numeric(lai$data)
lai = lai %>% 
  filter(data <=2012)

#Litterfall
litt = litt %>% 
  filter(data <=2012)

#Join Data
hy = hy %>% 
  unite(col = "id", c("parcela", "year"), sep = '_')

biomass = as.data.frame(biomass)#To work
biomass = biomass %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')
colnames(biomass) = c("id", "biomass", "dist")

lai = lai %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')

litt = litt %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')
colnames(litt) = c("id", "dist", "litter")

fuel = fuel %>% 
  unite(col = "id", c("parcela", "data"), sep = '_')
colnames(fuel) = c("id", "fuel", "dist")


#Transpor Hyperion data
hy2 = hy[,c(4,5,6)]
#hy2$id = as.character(hy2$id)

geti = function(x){
  z = hy2 %>% 
    na.omit() %>% 
    filter(index == x) %>% 
    group_by(id) %>% 
    summarise(value = median(value))
}

ari = geti("ari");colnames(ari) = c("id","ari")
evi = geti("evi2");colnames(evi) = c("id","evi")
ndvi = geti("ndvi");colnames(ndvi) = c("id","ndvi")
vari = geti("vari");colnames(vari) = c("id","vari")
vig = geti("vig");colnames(vig) = c("id","vig")
lwvi2 = geti("lwvi2");colnames(lwvi2) = c("id","lwvi2")
msi = geti("msi");colnames(msi) = c("id","msi")
ndii = geti("ndii");colnames(ndii) = c("id","ndii")
ndwi = geti("ndwi");colnames(ndwi) = c("id","ndwi")
pssr = geti("pssr");colnames(pssr) = c("id","pssr")
psri = geti("psri");colnames(psri) = c("id","psri")
sipi = geti("sipi");colnames(sipi) = c("id","sipi")
wbi = geti("wbi");colnames(wbi) = c("id","wbi")
pri = geti("pri");colnames(pri) = c("id","pri")
rendvi = geti("rendvi");colnames(rendvi) = c("id","rendvi")
nirv = geti("nirv"); colnames(nirv) = c("id","nirv")

indexs = cbind(evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv)
indexs = indexs[,c(1,2,4,8,10,12,14,16,18,20,22,24,26,28)]

#Join with field data
lai = lai[,c(-2)]
lai$id = as.character(lai$id)
lai$lai = lai$lai/10
colnames(lai) = c("id", "LAI")
lai2 = melt(lai)

litt = litt[,c(-2)]
litt$id = as.character(litt$id)
colnames(litt) = c("id", "Litterfall")
litt2 = melt(litt)

area = full_join(lai2, litt2, by = "id")
area = area[,c(-2,-4)]
colnames(area) = c('id', 'LAI', 'Litterfall')

df = full_join(area, indexs, by = "id")

df = df %>% 
  separate(id, c('plot','year'), sep = '_')

control = df %>% 
  filter(plot == "control")
control = control[,c(-1,-2)]

b3yr = df %>% 
  filter(plot == "b3yr")
b3yr = b3yr[,c(-1,-2)]

b1yr = df %>% 
  filter(plot == "b1yr")
b1yr = b1yr[,c(-1,-2)]


#Correlation by ggally
df2 = df[,c(-1)]
ggcorr(df2, geom = "circle", nbreaks = 8)

ggcorr(control, geom = "circle", nbreaks = 8)
ggcorr(b3yr, geom = "circle", nbreaks = 8)
ggcorr(b1yr, geom = "circle", nbreaks = 8)





