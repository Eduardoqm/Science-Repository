#Proccess and Analysis extracting - H1
#Eduardo Q Marques 

library(terra)

#Load data ---------------------------------------------------------------------

setwd("G:/My Drive/GEE_Max_Consecutive_VPD_Days_DP_Annual")
dir()

#Consecutive VPD days > 0.75 kPa
list_rst = list.files()
cons_vpd = rast(list_rst)
names(cons_vpd) = substr(list_rst, 29, 32)

#Driest Period length
dpm = rast("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/Rasters/Driest_Period_Nathalia/DP_length_months.tif")

#Mapbiomas
mb = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MapBiomas_2024_col10.tiff")
fire_freq = rast("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil/MB_Fire_frequency_1985_2025.tiff")


plot(cons_vpd)
plot(dpm)
plot(mb)
plot(fire_freq)
