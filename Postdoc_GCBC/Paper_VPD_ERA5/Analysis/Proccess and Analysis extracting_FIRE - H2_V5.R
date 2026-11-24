#Proccess and Analysis extracting (FIRE) - H2
#Eduardo Q Marques 24-11-2026

library(terra)
library(sf)
library(tidyverse)

#Load data ---------------------------------------------------------------------
#Driest Period length
vpd_zone = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/Amazon_cons_VPD_Zone.tif")
plot(vpd_zone)

#Mapbiomas
scf = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Forest_age_2024B.tif")
plot(scf)



#Amazonia limits
am = vect("G:/My Drive/Geodata/Vectors/Amazonia.shp")
plot(am, add = T)

#Clipping to Amazonia Biome ----------------------------------------------------
cons_vpd = mask(crop(cons_vpd, am), am)
vpd_95 = mask(crop(vpd_95, am), am)
vpd_mean = mask(crop(vpd_mean, am), am)
vpd_hour = mask(crop(vpd_hour, am), am)
vpd_zone = mask(crop(vpd_zone, am), am)
#scf = mask(crop(scf, am), am)