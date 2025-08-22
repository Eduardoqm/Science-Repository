#Convert rasters to 70 meters (Elias Paper)

#Eduardo Q Marques 10-07-2025 UpDated 22-08-2025


library(terra)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters")
dir()

base = rast("ECOSTRESS_EVAP_Annual_2022.tif")
secf = rast("MB_Forest_age_30m.tif")
fore = rast("Forest_30m.tif")
past = rast("Pasture_30m.tif")
lst_year = rast("LST_Landsat_Annual_2022_2023.tif")
lst_dry = rast("LST_Landsat_Dry_2022_2023.tif")
lst_wet = rast("LST_Landsat_Rainy_2022_2023.tif")

limit = vect("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Shapes/BR_Amazon_DrySeason_filtered.shp")

#base = rast("ECOSTRESS_Annual_2022_toy.tif")
#secf = rast("MB_Forest_age_30m_toy.tif")
#fore = rast("Forest_30m_toy.tif")
#past = rast("Pasture_30m_toy.tif")
#lst_an_2022 = rast("LST_Landsat_Annual_2022.tif")
#lst_an_2023 = rast("LST_Landsat_Annual_2023.tif")
#lst_dry_2022 = rast("LST_Landsat_Dry_2022.tif")
#lst_dry_2023 = rast("LST_Landsat_Dry_2023.tif")
#lst_wet_2022 = rast("LST_Landsat_Rainy_2022.tif")
#lst_wet_2023 = rast("LST_Landsat_Rainy_2023.tif")

#Make base only inside the study area ------------------------------------------
base2 = mask(crop(base, limit), limit)
plot(base2)

#Resample by base --------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/Resampled_70m")

secf2 = resample(secf, base, method = "average")
plot(base)
plot(secf2)
writeRaster(secf2, "MB_Forest_age_70m.tif")

fore2 = resample(fore, base, method = "average")
plot(fore2)
writeRaster(fore2, "Forest_70m.tif")

past2 = resample(past, base, method = "average")
plot(past2)
writeRaster(past2, "Pasture_70m.tif")

lst_an_2022b = resample(lst_year, base, method = "average")
plot(lst_an_2022b)
writeRaster(lst_an_2022b, "LST_Landsat_Annual_2022_2023_70m.tif")

lst_dry_2022b = resample(lst_dry, base, method = "average")
plot(lst_dry_2022b)
writeRaster(lst_dry_2022b, "LST_Landsat_Dry_2022_2023_70m.tif")

lst_wet_2022b = resample(lst_wet, base, method = "average")
plot(lst_wet_2022b)
writeRaster(lst_wet_2022b, "LST_Landsat_Wet_2022_2023_70m.tif")







