#Data Spera by municipaly

library(raster)
library(rgdal)

#Load data ========================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Geographic scripts/Extract info by munic_Matopiba")

img = raster("spera_latlong_2016.tif")
muni = readOGR("shape_2016.shp")

#Create column to id munic in shape data
muni@data$muni_id = c(1:1385)

#Extracting data
muni_ext = extract(img, muni, na.rm = T, df = T)
