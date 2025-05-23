#Extract Fraction info by XY Blowdown

#Eduardo Q Marques 03-09-2021

library(leaflet)
library(raster)
library(sp)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(reshape2)

#Load data =======================================
#Fractional Images
setwd('C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Fraction_Landsat/Mistura Espectral')
files1 <- list.files(pattern="_frac$") 
files2 <- list.files(pattern = "_frac.tif$")
files = c(files1, files2)

#Vectors
tree_loc <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes/Blowdown",layer="Blowdown_trees_XY")

area1 = readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes", layer = 'Polygon_A_B_C')

#Make Bare Substrate, PV and NPV stacks ======================================================
#Crop Tanguro limits
frac = brick(files[[1]]) #To get the crs
area1 = spTransform(area1, crs(frac))
tree_loc = spTransform(tree_loc, crs(frac))
frac_tang = crop(frac, area1)

#Bare Substrate
bs = frac_tang[[1:3]] #Sequence is the number of files
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[1]]
  bs[[x]] = crop(y, area1)
}
names(bs) = files

#Photosynthetic Vegetation
pv = frac_tang[[1:3]]
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[2]]
  pv[[x]] = crop(y, area1)
}
names(pv) = files

#Non-Photosynthetic Vegetation
npv = frac_tang[[1:3]]
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[3]]
  npv[[x]] = crop(y, area1)
}
names(npv) = files

#Extract values by tree points ================================================
#Bare Substrate
bs_df = raster::extract(bs, tree_loc)
colnames(bs_df) = c("2018", "2019-06", "2019-07")
bs_df2 = melt(bs_df)
bs_df2 = bs_df2[,-1]
colnames(bs_df2) = c("year", "values")

ggplot(bs_df2, aes(x=year, y=values))+
  geom_boxplot()+
  ggtitle("Bare Substrate")

#Photosynthetic Vegetation
pv_df = raster::extract(pv, tree_loc)
colnames(pv_df) = c("2018", "2019-06", "2019-07")
pv_df2 = melt(pv_df)
pv_df2 = pv_df2[,-1]
colnames(pv_df2) = c("year", "values")

ggplot(pv_df2, aes(x=year, y=values))+
  geom_boxplot()+
  ggtitle("Photosynthetic Vegetation")

#Non-Photosynthetic Vegetation
npv_df = raster::extract(npv, tree_loc)
colnames(npv_df) = c("2018", "2019-06", "2019-07")
npv_df2 = melt(npv_df)
npv_df2 = npv_df2[,-1]
colnames(npv_df2) = c("year", "values")

ggplot(npv_df2, aes(x=year, y=values))+
  geom_boxplot()+
  ggtitle("Non-Photosynthetic Vegetation")










plot(area1)
plot(tree_loc, col = tree_loc$tp_d_dn, add =  T)
