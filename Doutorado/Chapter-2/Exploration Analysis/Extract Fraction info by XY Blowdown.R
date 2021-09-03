#Extract Fraction info by XY Blowdown

#Eduardo Q Marques 03-09-2021

library(leaflet)
library(raster)
library(sp)
library(rgdal)

#Load data =======================================
#Fractional Images
setwd('C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Fraction_Landsat/Mistura Espectral')
dir()

files1 <- list.files(pattern="_frac$") 
files2 <- list.files(pattern = "_frac.tif$")
files = c(files1, files2)

frac = brick(files[[1]]) #To get the crs


#Vextors
tree_loc <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes/Blowdown",layer="Blowdown_trees_XY")

area1 = readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/shapes", layer = 'Polygon_A_B_C')

















plot(area1)
plot(tree_loc, col = tree_loc$tp_d_dn, add =  T)
