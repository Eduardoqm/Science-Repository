#Data Spera by municipaly

library(raster)
library(rgdal)
library(tidyverse)

#Load data ========================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Geographic scripts/Extract info by munic_Matopiba")

img = raster("spera_latlong_2016.tif")
#muni = readOGR("shape_2016.shp")
muni = readOGR("PAM_2019.csv.shp")


#Make tests with toy ========================================================================
#Reduce polygons to Tocantins
muni2 = muni[1:139,]
muni2@data$muni_id = c(1:length(muni2$MUNI))

#Separete Spera data in binary raster do wich class



#Create column to id munic in shape data


#Extracting data
muni_ext = extract(img, muni, na.rm = T, df = T)
