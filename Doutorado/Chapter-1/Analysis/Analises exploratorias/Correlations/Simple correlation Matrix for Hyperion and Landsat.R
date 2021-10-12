########################################
#Correlation Hyperion and Landsat - V2 #
#                                      #
#Eduardo Q Marques 21-09-2021          #
########################################


library(tidyverse)
library(reshape2)
library(ggplot2)
library(GGally)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Load data ===============================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')
land = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify and Filter Landsat data ==========================================
land$year = substr(land$year, 1,4)
land$year = as.numeric(land$year)
land$index = as.character(land$index)
land$treat = as.character(land$treat)
land$x = substr(land$x, 1,6)
land$y = substr(land$y, 1,6)

#Modify elements of dataframe
land$index[land$index == "evi2"] <- c("EVI")
land$index[land$index == "ndvi"] <- c("NDVI")
land$index[land$index == "ndii"] <- c("NDII")
land$index[land$index == "grnd"] <- c("GRND")
land$index[land$index == "nbr"] <- c("NBR")
land$index[land$index == "nbr2"] <- c("NBR2")
land$treat[land$treat == "control"] <- c("Controle")
land$treat[land$treat == "b3yr"] <- c("B3yr")
land$treat[land$treat == "b1yr"] <- c("B1yr")

#Resume repeat years with mean and select same years of Hyperion
land = land %>% 
  group_by(x, y, index, year, treat) %>% 
  summarise(value = mean(value)) 
  #filter(year %in% c(2004, 2005, 2006, 2008, 2010, 2011))

#Modify and Filter Hyperion data ==========================================
hy$year = as.numeric(hy$year)
hy$index = as.character(hy$index)
hy$treat = as.character(hy$treat)
hy$x = substr(hy$x, 1,6)
hy$y = substr(hy$y, 1,6)

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

#Summarize information to matching treatment and y distance ======================
land = land %>%
  na.omit() %>% 
  #unite("id", c("year", "x", "y"), sep = "_") %>% 
  unite("id", c("treat", "year"), sep = "_") %>% 
  group_by(id, index) %>% 
  summarise(value = median(value)) 

hy = hy %>%
  na.omit() %>% 
  #unite("id", c("year", "x", "y"), sep = "_") %>% 
  unite("id", c("treat", "year"), sep = "_") %>% 
  group_by(id, index) %>% 
  summarise(value = median(value)) 

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
evi_landsat = getix2("EVI");colnames(evi_landsat) = c("id","EVI")
ndvi_landsat = getix2("NDVI");colnames(ndvi_landsat) = c("id","NDVI")
ndii_landsat = getix2("NDII");colnames(ndii_landsat) = c("id","NDII")
nbr_landsat = getix2("NBR"); colnames(nbr_landsat) = c("id","NBR")
nbr2_landsat = getix2("NBR2"); colnames(nbr2_landsat) = c("id","NBR2")
grnd_landsat = getix2("GRND"); colnames(grnd_landsat) = c("id","GRND")

#Join Every Indice ================================================================
index_land = full_join(evi_landsat, ndvi_landsat, by = "id")
lista = list(ndii_landsat,nbr_landsat,nbr2_landsat,grnd_landsat)
for (x in 1:4) {
  index_land = full_join(index_land, lista[[x]], by = "id")
}


index_hy = full_join(evi, ndvi, by = "id")
lista = list(vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv,lwvi2,nbr,nbr2)
for (x in 1:15) {
  index_hy = full_join(index_hy, lista[[x]], by = "id")
}

#Calculate correlation ===========================================================
land_cor = ggcorr(index_land, label = TRUE)
hy_cor = ggcorr(index_hy, label = TRUE)

#ggsave(filename = "Corr_Landsat.png", plot = land_cor,
 #    path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Correlations", width = 15, height = 15, units =  "cm", dpi = 300)

#ggsave(filename = "Corr_Hyperion.png", plot = hy_cor,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Correlations", width = 30, height = 20, units =  "cm", dpi = 300)





