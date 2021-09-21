###################################
#Correlation Hyperion and Landsat #
#                                 #
#Eduardo Q Marques 20-09-2021     #
###################################


library(tidyverse)
library(reshape2)
library(ggplot2)

#Load data ===============================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')
land = read.csv("Landsat_indexs_all_xy.csv", sep = ',')


#Modify and Filter Landsat data ==========================================
land$year = substr(land$year, 1,4)
land$year = as.numeric(land$year)
land$index = as.character(land$index)
land$treat = as.character(land$treat)

#Modify elements of dataframe
land$index[land$index == "evi2"] <- c("EVI_landsat")
land$index[land$index == "ndvi"] <- c("NDVI_landsat")
land$index[land$index == "ndii"] <- c("NDII_landsat")
land$index[land$index == "grnd"] <- c("GRND_landsat")
land$index[land$index == "nbr"] <- c("NBR_landsat")
land$index[land$index == "nbr2"] <- c("NBR2_landsat")
land$treat[land$treat == "control"] <- c("Controle")
land$treat[land$treat == "b3yr"] <- c("B3yr")
land$treat[land$treat == "b1yr"] <- c("B1yr")

#Resume repeat years with mean and select same years of Hyperion
land = land %>% 
  group_by(x, y, index, year, treat) %>% 
  summarise(value = mean(value)) %>% 
  filter(year %in% c(2004, 2005, 2006, 2008, 2010, 2011))


#Modify and Filter Hyperion data ==========================================
hy$year = as.numeric(hy$year)
hy$index = as.character(hy$index)
hy$treat = as.character(hy$treat)

#Remove 2012 becouse Landsat dons't have
hy = hy %>%
  filter(year != 2012) %>% 
  filter(index != "ari") %>% 
  filter(index != "evi")

#Change names format
hy$index[hy$index == "evi2"] <- c("EVI")
hy$index[hy$index == "ndvi"] <- c("NDVI")
hy$index[hy$index == "ndii"] <- c("NDII")
hy$index[hy$index == "vig"] <- c("VIG")
hy$index[hy$index == "vari"] <- c("VARI")
hy$index[hy$index == "nirv"] <- c("NIRv")
hy$index[hy$index == "lwvi2"] <- c("LWVI2")
hy$index[hy$index == "msi"] <- c("MSI")
hy$index[hy$index == "ndwi"] <- c("NDWI")
hy$index[hy$index == "pssr"] <- c("PSSR")
hy$index[hy$index == "psri"] <- c("PSRI")
hy$index[hy$index == "sipi"] <- c("SIPI")
hy$index[hy$index == "wbi"] <- c("WBI")
hy$index[hy$index == "pri"] <- c("PRI")
hy$index[hy$index == "rendvi"] <- c("RENDVI")
hy$index[hy$index == "nbr"] <- c("NBR")
hy$index[hy$index == "nbr2"] <- c("NBR2")
hy$treat[hy$treat == "control"] <- c("Controle")
hy$treat[hy$treat == "b3yr"] <- c("B3yr")
hy$treat[hy$treat == "b1yr"] <- c("B1yr")

#Testing overlap ============================================================
hy_evi = hy %>% 
  filter(index == "EVI") %>% 
  filter(year == 2004)
land_evi = land %>% 
  filter(index == "EVI") %>% 
  filter(year == 2004)


ggplot()+
  geom_point(data = hy_evi, aes(x = x, y = y), col = "red", alpha = 0.3, size = 5)+
  geom_point(data = land_evi, aes(x = x, y = y), col = "blue", alpha = 0.3, size = 5)+
  coord_fixed()+
  theme_bw()

#Summarize information ===========================================================
land = land %>%
  na.omit() %>% 
  unite("id", c("treat", "year"), sep = "_") %>% 
  group_by(id, index) %>% 
  summarise(value = mean(value)) 

hy = hy %>%
  na.omit() %>% 
  unite("id", c("treat", "year"), sep = "_") %>% 
  group_by(id, index) %>% 
  summarise(value = mean(value)) 

#Transform inddices in columns ===================================================
getix = function(x){
  z = hy %>% 
    filter(index == x)
  z = z[,-2]
}

evi = getix("EVI");colnames(evi) = c("id","EVI")
ndvi = getix("NDVI");colnames(ndvi) = c("id","NDVI")
vari = getix("VARI");colnames(vari) = c("id","VARI")
vig = getix("VIG");colnames(vig) = c("id","VIG")
lwvi2 = getix("LWVI2");colnames(lwvi2) = c("id","LWVI2")
msi = getix("MSI");colnames(msi) = c("id","MSI")
ndii = getix("NDII");colnames(ndii) = c("id","NDII")
ndwi = getix("NDWI");colnames(ndwi) = c("id","NDWI")
pssr = getix("PSSR");colnames(pssr) = c("id","PSSR")
psri = getix("PSRI");colnames(psri) = c("id","PSRI")
sipi = getix("SIPI");colnames(sipi) = c("id","SIPI")
wbi = getix("WBI");colnames(wbi) = c("id","WBI")
pri = getix("PRI");colnames(pri) = c("id","PRI")
rendvi = getix("RENDVI");colnames(rendvi) = c("id","RENDVI")
nirv = getix("NIRv"); colnames(nirv) = c("id","NIRv")
nbr = getix("NBR"); colnames(nbr) = c("id","NBR")
nbr2 = getix("NBR2"); colnames(nbr2) = c("id","NBR2")

getix2 = function(x){
  z = land %>% 
    filter(index == x)
  z = z[,-2]
}
evi_landsat = getix2("EVI_landsat");colnames(evi_landsat) = c("id","EVI_landsat")
ndvi_landsat = getix2("NDVI_landsat");colnames(ndvi_landsat) = c("id","NDVI_landsat")
ndii_landsat = getix2("NDII_landsat");colnames(ndii_landsat) = c("id","NDII_landsat")
nbr_landsat = getix2("NBR_landsat"); colnames(nbr_landsat) = c("id","NBR_landsat")
nbr2_landsat = getix2("NBR2_landsat"); colnames(nbr2_landsat) = c("id","NBR2_landsat")
grnd_landsat = getix2("GRND_landsat"); colnames(grnd_landsat) = c("id","GRND_landsat")

indexs = cbind(evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv,lwvi2,nbr,nbr2,evi_landsat,ndvi_landsat,ndii_landsat,nbr_landsat,nbr2_landsat,grnd_landsat)

indexs = indexs[,c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46)]
colnames(indexs)[1] = c("id")

#Calculate correlation ===========================================================
library(GGally)

ggcorr(indexs, label = TRUE)

#ggpairs(indexs[,-1])+theme_bw()

res = ggcorr(indexs, label = TRUE)
df = res[["data"]]

df = df %>% 
  filter(x %in% c("EVI_landsat","NDVI_landsat","NDII_landsat","NBR_landsat","NBR2_landsat","grnd_landsat")) %>% 
  filter(y %in% c("EVI","NDVI","VARI","VIG","MSI","NDII","NDWI","PSSR","PSRI","SIPI","WBI","PRI","RENDVI","NIRv","LWVI2","NBR","NBR2"))


ggplot(df, aes(x, y, size = coefficient))+
  geom_point(aes(col = coefficient))+
  theme_bw()






#Ver: https://www.biostars.org/p/412100/














