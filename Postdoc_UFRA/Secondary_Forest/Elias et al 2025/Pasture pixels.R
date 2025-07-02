#Extracting Forest and Pasture from MapBiomas (Elias Paper)

#Eduardo Q Marques 30-06-2025

library(terra)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Postdoc_UFRA/Geodata/Rasters/MapBiomes_Brazil/Collection_9")
dir()

mb = rast("mb2023.tif")
mb

#Hold only Pasture -------------------------------------------------------------
#Make binary to count
mb2 = mb
mb2[mb2 != 15] = NA # Class Pasture for L3 (15)
mb2

#Saving
setwd("G:/Meu Drive/Postdoc_UFRA/Papers/Serra (Elias et al)/Analises_Elias/Rasters")
writeRaster(mb2, "Pasture_30m.tif")


#Hold only Forest --------------------------------------------------------------
#Make binary to count
mb3 = mb
mb3[mb3 != 3] = NA # Class Forest Formation for L3 (3)
mb3

#Saving
writeRaster(mb3, "Forest_30m.tif")