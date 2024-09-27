#Create points on desforestation by Hansen data

#Eduardo Q Marques  06-08-2024

library(terra)
library(sf)
library(sp)
library(tidyverse)

#Load data ---------------------------------------------------------------------
#has = rast("G:/My Drive/Postdoc_UFRA/Geodata/Rasters/PanAmazonia_Hansen_forest_loss_2000_2022.tif")
has = vect("G:/My Drive/Postdoc_UFRA/Geodata/Vectors/Amazonia_Hansen_forest_loss_dissolved.shp")
#pam = vect("G:/My Drive/Postdoc_UFRA/Geodata/Vectors/panamazonia.shp")
bio = vect("G:/My Drive/Postdoc_UFRA/Geodata/Vectors/biomas.shp")
buf = vect("G:/My Drive/Postdoc_UFRA/Papers/Amazonia_validation (Marques et al)/Shapes/Amazonia_point_buffer_10km.shp")

plot(has)
#plot(pam, add = T)
#plot(bio[5], add = T)
plot(buf, add = T)

#Crop dor Legal Amazonia
#has2 = crop(has, bio[5])
#has2 = mask(has2, bio[5])
#has2[has2 == 0] <- NA

#setwd(("G:/My Drive/Postdoc_UFRA/Geodata/Rasters"))
#writeRaster(has2, "Amazonia_Hansen_forest_loss_2000_2022.tif")

#Creating points
#has3 = terra::as.polygons(has2)
has2 = st_as_sf(has) #Convert to Spatial Feature
has3=sf::as_Spatial(has2$geom) #Conver to Saptial Polygons
plot(has3)

pontos = spsample(has3, n = 500, type = "hexagonal")
#plot(pontos, add = T)
pontos2 = st_as_sf(pontos)
plot(pontos2, col = "red", add = T)

setwd("G:/My Drive/Postdoc_UFRA/Papers/Amazonia_validation (Marques et al)/Shapes")
st_write(pontos2, "hansen_500_points.shp")

##

















has3 = terra::as.points(has2)
#has3 = st_as_sf(has3)


relate(has3, buf, "overlaps")


hasdell = intersect(has3, buf)

has3 = st_as_sf(has3)
hasdell = st_as_sf(hasdell)

plot(hasdell)
plot(has3)




