#Correlation Matrix Hyperion and full plot data
#By: Eduardo Q Marques 11-05-2020 Update in 31-08-2021
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
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")


hy = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')
#hy = hy[,c(-5, -9)] #NDWI have so high values in comparison with other indexs
hy$year = as.character(hy$year)

biomass = read.csv("Biomass_full_tang.csv", sep = ",", header = TRUE)

lai = read.csv("LAI_full_tang.csv", sep = ",", header = TRUE)

litt  = read.csv("Liteira_full_tang.csv", sep = ",", header = TRUE)

fuel = read.csv("Fuel_full_tang.csv", sep = ",", header = TRUE)

#Data organization =======================================================================
#From Hyperion we have 2004:2012, so let's try use other datas in this range
#Biomass
biomass = biomass %>% 
  filter(data <=2012)

#LAI
lai$data = as.numeric(lai$lai)
lai = lai %>% 
  filter(data <=2012)

#Litterfall
litt = litt %>% 
  filter(year <=2012)

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
hy2 = hy[,c(6,8,7)]
#Only for all indices ===========================================================================
getix = function(x){
  z = hy2 %>% 
    #na.omit() %>% 
    filter(index == x)
}

evi = getix("evi2");colnames(evi) = c("id","evi","id2")
ndvi = getix("ndvi");colnames(ndvi) = c("id","ndvi","id2")
vari = getix("vari");colnames(vari) = c("id","vari","id2")
vig = getix("vig");colnames(vig) = c("id","vig","id2")
lwvi2 = getix("lwvi2");colnames(lwvi2) = c("id","lwvi2","id2")
msi = getix("msi");colnames(msi) = c("id","msi","id2")
ndii = getix("ndii");colnames(ndii) = c("id","ndii","id2")
ndwi = getix("ndwi");colnames(ndwi) = c("id","ndwi","id2")
pssr = getix("pssr");colnames(pssr) = c("id","pssr","id2")
psri = getix("psri");colnames(psri) = c("id","psri","id2")
sipi = getix("sipi");colnames(sipi) = c("id","sipi","id2")
wbi = getix("wbi");colnames(wbi) = c("id","wbi","id2")
pri = getix("pri");colnames(pri) = c("id","pri","id2")
rendvi = getix("rendvi");colnames(rendvi) = c("id","rendvi","id2")
nirv = getix("nirv"); colnames(nirv) = c("id","nirv","id2")
nbr = getix("nbr"); colnames(nbr) = c("id","nbr","id2")
nbr2 = getix("nbr2"); colnames(nbr2) = c("id","nbr2","id2")

indexs2 = cbind(evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv,lwvi2,nbr,nbr2)
indexs2 = indexs2[,c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50)]

ggcorr(indexs2, label = TRUE)

ggpairs(indexs2)+
  theme_bw()

#Use ggpairs
indexs3 = cbind(hy$id, indexs2)
indexs3 = indexs3 %>% 
  separate(col = "hy$id", c("Parcela", "year"), sep = '_')

X11()
p = ggpairs(indexs3, columns = 3:17, ggplot2::aes(color=Parcela))+
  theme_bw()

for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("orange", "red", "blue")) +
      scale_color_manual(values=c("orange", "red", "blue"))  
  }
}
p 
