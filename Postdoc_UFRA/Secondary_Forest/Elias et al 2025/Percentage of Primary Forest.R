#Percentage of Primary Forest (Elias Paper)

#Eduardo Q Marques 26-02-2025

library(terra)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serra (Elias et al)/Analises_Elias/Rasters")
dir()

base = rast("ET_Amazonia_2023_1km.tif")
secfor = rast("Perc_SecForest_1km.tif")
mb = rast("MB2023_l1_30m.tiff")

#Calculating percentage of primary forest ------------------------------------
#Make binary to count
mb2 = mb
mb2[mb2 != 1] = 0 # Class Forest for L1 (1)

ttl = mb #Total of pixels to extract fraction
ttl[ttl != 1] = 1


#To understand resample -> https://rdrr.io/cran/terra/man/resample.html
mb3 = resample(mb2, base, method = "sum")
ttl2 = resample(ttl, base, method = "sum")

mb4 = (mb3/ttl2)*100

#Substract the Secondary Forest percentage
#mb5 = mb4 - secfor
#mb5[mb5 < 0] = NA

plot(mb)
#plot(mb4, add = T)
plot(mb5, add = T)

writeRaster(mb5, "Perc_PriForest_1km.tif")
