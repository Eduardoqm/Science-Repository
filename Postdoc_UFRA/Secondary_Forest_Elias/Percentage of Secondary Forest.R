#Mean of Secondary Forest age (Elias Paper)

#Eduardo Q Marques 24-02-2025


library(terra)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo/Documents/Analises_Elias/Rasters")
dir()

base = rast("ET_Amazonia_2023_1km.tif")
secf = rast("MB_Forest_age_30m.tif")

#Resample by secondary forest age ----------------------------------------------
m_age = resample(secf, base, method = "average")

plot(secf)
plot(m_age, add = T)

writeRaster(m_age, "Mean_Forest_age_1km.tif")