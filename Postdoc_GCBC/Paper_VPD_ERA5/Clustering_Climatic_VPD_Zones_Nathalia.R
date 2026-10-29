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

