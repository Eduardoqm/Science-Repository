#Clustering Climatic Zones of Amazon by VPD consecutive days > 0.75

#Eduardo Q Marques 29-10-2026

library(terra)
library(tidyverse)

#Functions ---------------------------------------------------------------------
brick = function(w, a, b){
  setwd(w)
  print(dir())
  list_rst = list.files()
  z = rast(list_rst)
  names(z) = substr(list_rst, a, b)
  #plot(z)
  return(z)
}

#Load data ---------------------------------------------------------------------
#VPD rasters
cons_vpd = brick("G:/My Drive/GEE_Max_Consecutive_VPD_Days_DP_Annual", 29, 32) #Consecutive VPD days > 0.75 kPa

plot(cons_vpd[[51]])

cons_vpd2 = mean(cons_vpd)
plot(cons_vpd2)

#Clipping to Amazonia Biome ----------------------------------------------------
am = vect("G:/My Drive/Geodata/Vectors/Amazonia.shp")
plot(am, add = T)

cons_vpd3 = mask(crop(cons_vpd2, am), am)
plot(cons_vpd3)
freq(cons_vpd3)

#Classifying -------------------------------------------------------------------
vpd_zone[vpd_zone < 1] <- 0
vpd_zone[vpd_zone >= 1 & vpd_zone <= 3] <- 1
vpd_zone[vpd_zone > 3 & vpd_zone < 6] <- 2
vpd_zone[vpd_zone >= 6] <- 3

plot(vpd_zone)




















