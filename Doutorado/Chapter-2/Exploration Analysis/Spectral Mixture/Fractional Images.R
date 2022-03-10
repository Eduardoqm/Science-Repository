#Fractional Images

#Eduardo Q Marques 15-05-2020

library(raster)
library(rasterVis)
library(rgdal)
library(viridis)

#Load data
setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Fraction_Landsat')
dir()

frac = brick("L8_224069_20140705_frac")

area1 = readOGR(dsn = 'C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Shapes', layer = 'Polygon_A_B_C')

#Plot data
levelplot(frac, margin = FALSE, col.regions = viridis(100))

#Crop for the stude area
area1 = spTransform(area1, crs(frac))
frac = crop(frac, area1)

#Plot again
levelplot(frac, margin = FALSE, col.regions = viridis(100))

levelplot(frac[[3]], margin = FALSE, col.regions = viridis(100))
