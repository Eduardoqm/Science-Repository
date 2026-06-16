#Calculating 2022-2024 means (Wet)
#Eduardo Q Marques 16-06-2026

library(terra)

#setwd("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m")
setwd("C:/Users/Workshop/Documents/Dados Elias") #From Workshop Leptop
dir()

#LST
lst22 <- rast("LST_Landsat_Wet_2022_70m.tif")
lst23 <- rast("LST_Landsat_Wet_2023_70m.tif")
lst24 <- rast("LST_Landsat_Wet_2024_70m.tif")

lst_cur = mean(lst22, lst23, lst24, na.rm = T)
plot(lst_cur)

#ET
et22 <- rast("ECOSTRESS_ET_Wet_2022_70m.tif")
et23 <- rast("ECOSTRESS_ET_Wet_2023_70m.tif")
et24 <- rast("ECOSTRESS_ET_Wet_2024_70m.tif")

et_cur = mean(et22, et23, et24, na.rm = T)
plot(et_cur)

#setwd("G:/Meu Drive/Dados_Elias_paper/LST_ET_scenario")
setwd("G:/My Drive/Research/Papers/Serrapilheira (Elias et al)/Scenery")#From Workshop Leptop

writeRaster(lst_cur, "LST_Wet_current.tif")
writeRaster(et_cur, "ET_Wet_current.tif")

