#Percentage of Secondary Forest (Elias Paper)

#Eduardo Q Marques 24-02-2025

library(terra)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo/Documents/Analises_Elias/Rasters")
dir()

base = rast("ET_Amazonia_2023_1km.tif")
secf = rast("MB_Forest_age_30m.tif")

#Calculating percentage of secondary forest ------------------------------------
secf2 = secf

secf2[secf2 > 0] = 1
secf2[is.na(secf2)] = 0



#Resample by not traditional way -----------------------------------------------
secf_3 = resample(secf2, base, method = "sum")





base2 <- as.polygons(base, fun=function(x){x>=0})

#Counting
secf3 = terra::extract(secf2, base2[1,], fun = count, df = F, na.rm = T)







secf_3 = resample(secf2, base, method = "count")




plot(secf2)
plot(m_age, add = T)

#writeRaster(m_age, "Perc_SecForest_1km.tif")