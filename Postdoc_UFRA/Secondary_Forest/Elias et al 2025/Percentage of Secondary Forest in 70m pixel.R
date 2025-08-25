#Percentage of Secondary Forest in 70m pixel (Elias Paper)

#Eduardo Q Marques 10-07-2025

library(terra)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters")
dir()

base = rast("ECOSTRESS_EVAP_Annual_2022.tif")
secf = rast("MB_Forest_age_30m.tif")

limit = vect("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Shapes/BR_Amazon_DrySeason_filtered.shp")

#Cropping rasters --------------------------------------------------------------
base = mask(crop(base, limit), limit)
plot(base)

secf = mask(crop(secf, limit), limit)
plot(secf)

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

setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serrapilheira (Elias et al)/Analises_Elias/Rasters/Resampled_70m")
writeRaster(secf4, "Perc_SecForest_70m.tif")
