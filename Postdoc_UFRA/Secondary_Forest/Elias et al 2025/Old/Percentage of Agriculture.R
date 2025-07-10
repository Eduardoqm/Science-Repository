#Percentage of Agriculture (Elias Paper)

#Eduardo Q Marques 27-02-2025

library(terra)

#Load data ---------------------------------------------------------------------
setwd("C:/Users/Eduardo/Documents/Analises_Elias/Rasters")
dir()

base = rast("ET_Amazonia_2023_1km.tif")
mb = rast("MB2023_l1_30m.tiff")

#Calculating percentage of Agriculture -----------------------------------------
#Make binary to count
mb2 = mb
mb2[mb2 != 14] = 0 # Class Agriculture for L1 (14)
mb2

ttl = mb #Total of pixels to extract fraction
ttl[ttl != 1] = 1


#To understand resample -> https://rdrr.io/cran/terra/man/resample.html
mb3 = resample(mb2, base, method = "sum")
ttl2 = resample(ttl, base, method = "sum")

mb4 = ((mb3/ttl2)*100)/14

plot(mb)
plot(mb4, add = T)

writeRaster(mb4, "Perc_Agriculture_1km.tif")