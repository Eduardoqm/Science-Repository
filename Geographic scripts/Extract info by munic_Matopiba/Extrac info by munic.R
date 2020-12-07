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

#Create column to id munic in shape data
muni2@data$muni_id = c(1:length(muni2$MUNI))

#Separete Spera data in binary raster do wich class
cls1 = img #Soy single cropped
cls1[cls1 != 1] = 0

cls2 = img #Corn single cropped
cls2[cls2 != 2] = 0; cls2[cls2 == 2] = 1

cls4 = img #Cotton single cropped
cls4[cls4 != 4] = 0; cls4[cls4 == 4] = 1

cls5 = img #Soy/Cotton double crop rotation
cls5[cls5 != 5] = 0; cls5[cls5 == 5] = 1

cls6 = img #Soy/Corn double crop roation
cls6[cls != 6] = 0; cls6[cls == 6] = 1


#Extracting data
muni_c1 = extract(cls1, muni2, na.rm = T, df = T)
