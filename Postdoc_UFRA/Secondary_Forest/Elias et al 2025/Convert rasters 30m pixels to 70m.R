#Convert rasters to 70 meters (Elias Paper)

#Eduardo Q Marques 10-07-2025


library(terra)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/Toy/raster")
dir()

base = rast("ECOSTRESS_Annual_2022_toy.tif")
secf = rast("MB_Forest_age_30m_toy.tif")
fore = rast("Forest_30m_toy.tif")
past = rast("Pasture_30m_toy.tif")
lst_an_2022 = rast("LST_Landsat_Annual_2022.tif")
lst_an_2023 = rast("LST_Landsat_Annual_2023.tif")
lst_dry_2022 = rast("LST_Landsat_Dry_2022.tif")
lst_dry_2023 = rast("LST_Landsat_Dry_2023.tif")
lst_wet_2022 = rast("LST_Landsat_Rainy_2022.tif")
lst_wet_2023 = rast("LST_Landsat_Rainy_2023.tif")

#Resample by base --------------------------------------------------------------
secf2 = resample(secf, base, method = "average")
plot(base)
plot(secf2)
writeRaster(secf2, "MB_Forest_age_70m_toy.tif")

fore2 = resample(fore, base, method = "average")
plot(fore2)
writeRaster(fore2, "Forest_70m_toy.tif")

past2 = resample(past, base, method = "average")
plot(past2)
writeRaster(past2, "Pasture_70m_toy.tif")

lst_an_2022b = resample(lst_an_2022, base, method = "average")
plot(lst_an_2022b)
writeRaster(lst_an_2022b, "LST_Landsat_Annual_2022_70m.tif")

lst_an_2023b = resample(lst_an_2023, base, method = "average")
plot(lst_an_2023b)
writeRaster(lst_an_2023b, "LST_Landsat_Annual_2023_70m.tif")


lst_dry_2022b = resample(lst_dry_2022, base, method = "average")
plot(lst_dry_2022b)
writeRaster(lst_dry_2022b, "LST_Landsat_Dry_2022_70m.tif")

lst_dry_2023b = resample(lst_dry_2023, base, method = "average")
plot(lst_dry_2023b)
writeRaster(lst_dry_2023b, "LST_Landsat_Dry_2023_70m.tif")


lst_wet_2022b = resample(lst_wet_2022, base, method = "average")
plot(lst_wet_2022b)
writeRaster(lst_wet_2022b, "LST_Landsat_Wet_2022_70m.tif")

lst_wet_2023b = resample(lst_wet_2023, base, method = "average")
plot(lst_wet_2023b)
writeRaster(lst_wet_2023b, "LST_Landsat_Wet_2023_70m.tif")





