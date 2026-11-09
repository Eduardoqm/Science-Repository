#Paper VPD ERA5 Amazon - Proccess to extract info from 2023 and 2025 SF

#Eduardo Q Marques 09-11-2026

library(terra)
library(tidyverse)

#Load data ---------------------------------------------------------------------
#Secondary Forest
setwd("G:/My Drive/Geodata/Rasters/MapBiomes_Brazil")
scf24 = rast("MB_Forest_age_2024B.tif") #Leptop
plot(scf24)

#2023 --------------------------------------------------------------------------
scf23 = scf24 - 1
scf23 = ifel(scf23 < 1, NA, scf23)
plot(scf23)

writeRaster(scf23, "MB_Forest_age_2023B.tif")

#2025 --------------------------------------------------------------------------