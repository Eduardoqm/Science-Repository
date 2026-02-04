#Calculating 2022-2024 means (Annual)
#Eduardo Q Marques 12-01-2026

library(terra)

setwd("C:/Users/Public/Documents/Analises_Elias/Rasters/Resampled_70m")
dir()

#LST
lst22 <- rast("LST_Landsat_Annual_2022_70m.tif")
lst23 <- rast("LST_Landsat_Annual_2023_70m.tif")
lst24 <- rast("LST_Landsat_Annual_2024_70m.tif")

lst_cur = mean(lst22, lst23, lst24, na.rm = T)
plot(lst_cur)

#ET
et22 <- rast("ECOSTRESS_ET_Annual_2022_70m.tif")
et23 <- rast("ECOSTRESS_ET_Annual_2023_70m.tif")
et24 <- rast("ECOSTRESS_ET_Annual_2024_70m.tif")

et_cur = mean(et22, et23, et24, na.rm = T)
plot(et_cur)

setwd("G:/Meu Drive/Dados_Elias_paper/LST_ET_scenario")
writeRaster(lst_cur, "LST_Annual_current.tif")
writeRaster(et_cur, "ET_Annual_current.tif")


#Deltas
setwd("G:/Meu Drive/Dados_Elias_paper/Delta_Raster")
dir()

rst = list.files()

delta_et_f = mean(rast(rst[1]), rast(rst[2]), rast(rst[3]), na.rm = T); plot(delta_et_f)

delta_et_p = mean(rast(rst[19]), rast(rst[20]), rast(rst[21]), na.rm = T); plot(delta_et_p)

delta_lst_f = mean(rast(rst[37]), rast(rst[38]), rast(rst[39]), na.rm = T); plot(delta_lst_f)

delta_lst_p = mean(rast(rst[55]), rast(rst[56]), rast(rst[57]), na.rm = T); plot(delta_lst_p)


setwd("G:/Meu Drive/Dados_Elias_paper/LST_ET_scenario")
writeRaster(delta_et_f, "Forest_Delta_ET_annual.tif")
writeRaster(delta_et_p, "Pasture_Delta_ET_annual.tif")
writeRaster(delta_lst_f, "Forest_Delta_LST_annual.tif")
writeRaster(delta_lst_p, "Pasture_Delta_LST_annual.tif")


