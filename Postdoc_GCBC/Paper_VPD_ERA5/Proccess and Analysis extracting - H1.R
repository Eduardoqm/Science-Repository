#Proccess and Analysis extracting - H1
#Eduardo Q Marques 

library(terra)

#Functions ---------------------------------------------------------------------
brick = function(w){
  setwd(w)
  print(dir())
  list_rst = list.files()
  z = rast(list_rst)
  names(z) = substr(list_rst, 29, 32)
  plot(z)
  return(z)
}

#Load data ---------------------------------------------------------------------
#VPD rasters
cons_vpd = brick("G:/My Drive/GEE_Max_Consecutive_VPD_Days_DP_Annual") #Consecutive VPD days > 0.75 kPa
vpd_95 = brick("G:/My Drive/GEE_Q95_VPD_DP_Annual") #Annual quantile 95 VDP
vpd_mean = brick("G:/My Drive/GEE_Mean_VPD_DP_Annual") #Annual mean VDP

#Driest Period length
dpm = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/DP_length_months.tif")
plot(dpm)

#Mapbiomas
mb = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MapBiomas_2024_col10.tiff")
fire_freq = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Fire_frequency_1985_2025.tiff")
plot(mb)
plot(fire_freq)
