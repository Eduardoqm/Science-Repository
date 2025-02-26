#Percentage of Secondary Forest (Elias Paper)

#Eduardo Q Marques 24-02-2025

library(terra)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo/Documents/Analises_Elias/Rasters")
dir()

base = rast("ET_Amazonia_2023_1km.tif")
secf = rast("MB_Forest_age_30m.tif")

#Calculating percentage of secondary forest ------------------------------------
#Make binary to count
secf2 = secf
secf2[secf2 > 0] = 1
secf2[is.na(secf2)] = 0

ttl = secf2 #Total of pixels to extract fraction
ttl[ttl == 0] = 1

#To understand resample -> https://rdrr.io/cran/terra/man/resample.html
secf3 = resample(secf2, base, method = "sum", threads = T)

ttl2 = resample(ttl, base, method = "sum", threads = T)

secf4 = (secf3/ttl2)*100


plot(secf4)

writeRaster(secf4, "Perc_SecForest_1km.tif")