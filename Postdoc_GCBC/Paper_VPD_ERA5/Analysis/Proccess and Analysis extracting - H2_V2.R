#Proccess and Analysis extracting - H2
#Eduardo Q Marques 16-11-2026

library(terra)
library(tidyverse)

#Load data ---------------------------------------------------------------------
#VPD rasters (2024)
cons_vpd = rast("G:/My Drive/GEE_Max_Consecutive_VPD_Days_DP_Annual/max_consecutive_vpd_days_DP_2024.tif") #Consecutive VPD days > 0.75 kPa
vpd_95 = rast("G:/My Drive/GEE_Q95_VPD_DP_Annual/q95_vpd_dp_2024.tif") #Annual quantile 95 VDP
vpd_mean = rast("G:/My Drive/GEE_Mean_VPD_DP_Annual/mean_vpd_dp_2024.tif") #Annual mean VDP
vpd_hour = rast("G:/My Drive/GEE_Annual_VPD_Hours_DP/annual_vpd_hours_DP_2024.tif") #Hours VPD > 0.75 kPa

plot(cons_vpd)
plot(vpd_95)
plot(vpd_mean)
plot(vpd_hour)

#Driest Period length
vpd_zone = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/Amazon_cons_VPD_Zone.tif")
vpd_zone
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
scf = mask(crop(scf, am), am)

