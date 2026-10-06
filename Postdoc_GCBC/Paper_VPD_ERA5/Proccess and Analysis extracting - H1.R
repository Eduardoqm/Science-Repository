#Proccess and Analysis extracting - H1
#Eduardo Q Marques 

library(terra)
library(tidyverse)

#Functions ---------------------------------------------------------------------
brick = function(w, a, b){
  setwd(w)
  print(dir())
  list_rst = list.files()
  z = rast(list_rst)
  names(z) = substr(list_rst, a, b)
  plot(z)
  return(z)
}

#Load data ---------------------------------------------------------------------
#VPD rasters
cons_vpd = brick("G:/My Drive/GEE_Max_Consecutive_VPD_Days_DP_Annual", 29, 32) #Consecutive VPD days > 0.75 kPa
vpd_95 = brick("G:/My Drive/GEE_Q95_VPD_DP_Annual", 12, 15) #Annual quantile 95 VDP
vpd_mean = brick("G:/My Drive/GEE_Mean_VPD_DP_Annual", 13, 16) #Annual mean VDP

#Driest Period length
dpm = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/DP_length_months.tif")
plot(dpm)

#Mapbiomas
mb = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MapBiomas_2024_col10.tiff")
fire_freq = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Fire_frequency_1985_2025.tiff")
plot(mb)
plot(fire_freq)

#Time series -------------------------------------------------------------------

for (z in 1:length(cons_vpd@ptr@.xData)) {
  plot(cons_vpd[[z]])
}

df = as.data.frame(cons_vpd[[1]])
colnames(df) = "cons_vpd"
df$year = names(cons_vpd[[1]])

for (z in 2:51) {
  print(names(cons_vpd[[z]]))
  df2 = as.data.frame(cons_vpd[[z]])
  colnames(df2) = "cons_vpd"
  df2$year = names(cons_vpd[[z]])
  df = rbind(df, df2)
}

df$year = as.numeric(df$year)

ggplot(df, aes(x=year, y=cons_vpd))+
  geom_smooth()






