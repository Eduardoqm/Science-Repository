#Data Spera by municipaly

library(raster)
library(rgdal)

#Load data ========================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Geographic scripts/Extract info by munic_Matopiba")

img = raster("spera_latlong_2016.tif")
muni = readOGR("shape_2016.shp")

