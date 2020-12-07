#Data Spera by municipaly

library(raster)
library(rgdal)
library(tidyverse)

#Load data ==================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Programas/R/Science-Repository/Geographic scripts/Extract info by munic_Matopiba")

img = raster("spera_latlong_2016.tif")
muni = readOGR("Matopiba_munic.shp")


#Create column to id munic in shape data ====================================================
muni@data$muni_id = c(1:length(muni$MUNI))

#Separete Spera data in binary raster do wich class =========================================
cls1 = img #Soy single cropped
cls1[cls1 != 1] = 0

cls2 = img #Corn single cropped
cls2[cls2 != 2] = 0; cls2[cls2 == 2] = 1

cls4 = img #Cotton single cropped
cls4[cls4 != 4] = 0; cls4[cls4 == 4] = 1

cls5 = img #Soy/Cotton double crop rotation
cls5[cls5 != 5] = 0; cls5[cls5 == 5] = 1

cls6 = img #Soy/Corn double crop roation
cls6[cls6 != 6] = 0; cls6[cls6 == 6] = 1


#Extracting data ============================================================================
c1 = raster::extract(cls1, muni, na.rm = T, df = T, fun = sum)
c2 = raster::extract(cls2, muni, na.rm = T, df = T, fun = sum)
c4 = raster::extract(cls4, muni, na.rm = T, df = T, fun = sum)
c5 = raster::extract(cls5, muni, na.rm = T, df = T, fun = sum)
c6 = raster::extract(cls6, muni, na.rm = T, df = T, fun = sum)

#Bind classes data with vector data =========================================================
colnames(c1) = c("muni_id", "class1")
colnames(c2) = c("muni_id", "class2")
colnames(c4) = c("muni_id", "class4")
colnames(c5) = c("muni_id", "class5")
colnames(c6) = c("muni_id", "class6")

muni@data = cbind(muni@data, c1, c2, c4, c5, c6)

#Cleaning vector data =======================================================================
muni@data = muni@data[,c(-7,-9,-11,-13,-15)]

#Export Shape ===============================================================================
writeOGR(muni, layer = "Munic_Spera_2016", driver = "ESRI Shapefile")






