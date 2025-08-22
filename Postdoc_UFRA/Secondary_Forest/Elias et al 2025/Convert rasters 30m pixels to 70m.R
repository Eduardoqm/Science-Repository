#Convert rasters to 70 meters (Elias Paper)

#Eduardo Q Marques 10-07-2025 UpDated 22-08-2025


library(terra)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters")
dir()

base = rast("ECOSTRESS_EVAP_Annual_2022.tif")
evp_dry = rast("ECOSTRESS_EVAP_DrySeason_2022.tif")
evp_wet = rast("ECOSTRESS_EVAP_WetSeason_2022.tif")
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

#Resample by base --------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/Resampled_70m")

base2 = mask(crop(base, limit), limit)
plot(base2)
writeRaster(base2, "ECOSTRESS_EVAP_Annual_2022_70m.tif")

evp_dry2 = resample(evp_dry, base2, method = "average")
evp_dry2 = mask(crop(evp_dry2, limit), limit)
plot(evp_dry2)
writeRaster(evp_dry2, "ECOSTRESS_EVAP_DrySeason_2022_70m.tif")

evp_wet2 = resample(evp_wet, base2, method = "average")
evp_wet2 = mask(crop(evp_wet2, limit), limit)
plot(evp_wet2)
writeRaster(evp_wet2, "ECOSTRESS_EVAP_WetSeason_2022_70m.tif")

secf2 = resample(secf, base2, method = "average")
secf2 = mask(crop(secf2, limit), limit)
plot(secf2)
writeRaster(secf2, "MB_Forest_age_70m.tif")

fore2 = resample(fore, base2, method = "average")
fore2 = mask(crop(fore2, limit), limit)
plot(fore2)
writeRaster(fore2, "Forest_70m.tif")

past2 = resample(past, base2, method = "average")
past2 = mask(crop(past2, limit), limit)
plot(past2)
writeRaster(past2, "Pasture_70m.tif")

lst_year2 = resample(lst_year, base2, method = "average")
lst_year2 = mask(crop(lst_year2, limit), limit)
plot(lst_year2)
writeRaster(lst_year2, "LST_Landsat_Annual_2022_2023_70m.tif")

lst_dry2 = resample(lst_dry, base2, method = "average")
lst_dry2 = mask(crop(lst_dry2, limit), limit)
plot(lst_dry2)
writeRaster(lst_dry2, "LST_Landsat_Dry_2022_2023_70m.tif")

lst_wet2 = resample(lst_wet, base2, method = "average")
lst_wet2 = mask(crop(lst_wet2, limit), limit)
plot(lst_wet2)
writeRaster(lst_wet2, "LST_Landsat_Wet_2022_2023_70m.tif")







