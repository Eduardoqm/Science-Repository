#Convert rasters to 70 meters (Elias Paper)

#Eduardo Q Marques 10-07-2025


library(terra)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/Toy")
dir()

base = rast("LST_Landsat_Annual_2022.tif")
secf = rast("MB_Forest_age_30m_toy.tif")
fore = rast("Forest_30m_toy.tif")
past = rast("Pasture_30m_toy.tif")

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
