#Extract Fraction info by XY Blowdown

#Eduardo Q Marques 03-09-2021

library(leaflet)
library(raster)
library(sp)
library(rgdal)

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
bs = frac_tang
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[1]]
  bs[[x]] = crop(y, area1)
}
names(bs) = files

#Photosynthetic Vegetation
pv = frac_tang
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[2]]
  pv[[x]] = crop(y, area1)
}
names(pv) = files

#Non-Photosynthetic Vegetation
npv = frac_tang
for (x in 1:length(files)) {
  z = brick(files[[x]])
  y = z[[3]]
  npv[[x]] = crop(y, area1)
}
names(npv) = files















plot(area1)
plot(tree_loc, col = tree_loc$tp_d_dn, add =  T)
