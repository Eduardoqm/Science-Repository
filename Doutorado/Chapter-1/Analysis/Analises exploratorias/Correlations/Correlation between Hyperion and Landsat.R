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




#Calculate correlation ===========================================================
getcor = function(x,w){
  z = hy %>% 
    na.omit() %>% 
    filter(index == x)
    
  k = land %>%
    na.omit() %>% 
    filter(index == w)
  
  print(cor(z$value, k$value), method = "pearson")
}

getcor("VARI","GRND")













#Ver: https://www.biostars.org/p/412100/














