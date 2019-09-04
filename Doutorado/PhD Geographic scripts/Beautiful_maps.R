#Beautiful maps to show in presentations
#Eduardo Q Marques 10/08/2019

library(raster)
library(rasterVis)
library(rgdal)

#Vectors
area1 = readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Area1big")
plots = readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C")

#NDVI
a = stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/NDVI", pattern = ".tif$", full.names=TRUE,recursive=TRUE))
mapa = stack(a[[33]], a[[35]], a[[39]], a[[45]], a[[52]], a[[21]])
names(mapa) = c('2004', '2005', '2007', '2008', '2011', '2019')

#Transform vectors
area1 = spTransform(area1, crs(mapa))
plots = spTransform(plots, crs(mapa))

#Crop rasters
mapa = crop(mapa, area1)

b = levelplot(mapa, margin = FALSE)
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa, margin = FALSE, par.settings = RdBuTheme, main = "NDVI 2004")
b + layer(sp.lines(plots, lwd=2.0, col='black'))


b = levelplot(mapa[[1]], margin = FALSE, par.settings = RdBuTheme, main = "NDVI 2004")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[2]], margin = FALSE, par.settings = RdBuTheme, main = "NDVI 2005")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[3]], margin = FALSE, par.settings = RdBuTheme, main = "NDVI 2008")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[4]], margin = FALSE, par.settings = RdBuTheme, main = "NDVI 2011")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[5]], margin = FALSE, par.settings = RdBuTheme, main = "NDVI 2019")
b + layer(sp.lines(plots, lwd=2.0, col='black'))



#EVI
a = stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/EVI2", pattern = ".tif$", full.names=TRUE,recursive=TRUE))
mapa = stack(a[[33]], a[[35]], a[[39]], a[[45]], a[[52]], a[[21]])
names(mapa) = c('2004', '2005', '2007', '2008', '2011', '2019')

#Crop rasters
mapa = crop(mapa, area1)

b = levelplot(mapa, margin = FALSE)
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[1]], margin = FALSE, par.settings = RdBuTheme, main = "EVI 2004")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[2]], margin = FALSE, par.settings = RdBuTheme, main = "EVI 2005")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[3]], margin = FALSE, par.settings = RdBuTheme, main = "EVI 2008")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[4]], margin = FALSE, par.settings = RdBuTheme, main = "EVI 2011")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[5]], margin = FALSE, par.settings = RdBuTheme, main = "EVI 2019")
b + layer(sp.lines(plots, lwd=2.0, col='black'))


#NDMI
a = stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/NDMI", pattern = ".tif$", full.names=TRUE,recursive=TRUE))
mapa = stack(a[[33]], a[[35]], a[[45]], a[[52]], a[[21]])
names(mapa) = c('2004', '2005', '2008', '2011', '2019')

#Crop rasters
mapa = crop(mapa, area1)

b = levelplot(mapa[[1]], margin = FALSE, par.settings = RdBuTheme, main = "NDMI 2004")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[2]], margin = FALSE, par.settings = RdBuTheme, main = "NDMI 2005")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[3]], margin = FALSE, par.settings = RdBuTheme, main = "NDMI 2008")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[4]], margin = FALSE, par.settings = RdBuTheme, main = "NDMI 2011")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[5]], margin = FALSE, par.settings = RdBuTheme, main = "NDMI 2019")
b + layer(sp.lines(plots, lwd=2.0, col='black'))


#GRND
a = stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/GRND", pattern = ".tif$", full.names=TRUE,recursive=TRUE))
mapa = stack(a[[33]], a[[35]], a[[45]], a[[52]], a[[21]])
names(mapa) = c('2004', '2005', '2008', '2011', '2019')

#Crop rasters
mapa = crop(mapa, area1)

b = levelplot(mapa[[1]], margin = FALSE, par.settings = RdBuTheme, main = "GRND 2004")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[2]], margin = FALSE, par.settings = RdBuTheme, main = "GRND 2005")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[3]], margin = FALSE, par.settings = RdBuTheme, main = "GRND 2008")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[4]], margin = FALSE, par.settings = RdBuTheme, main = "GRND 2011")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[5]], margin = FALSE, par.settings = RdBuTheme, main = "GRND 2019")
b + layer(sp.lines(plots, lwd=2.0, col='black'))


#NDWI
a = stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/NDWI", pattern = ".tif$", full.names=TRUE,recursive=TRUE))
mapa = stack(a[[33]], a[[35]], a[[45]], a[[52]], a[[21]])
names(mapa) = c('2004', '2005', '2008', '2011', '2019')

#Crop rasters
mapa = crop(mapa, area1)

b = levelplot(mapa[[1]], margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2004")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[2]], margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2005")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[3]], margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2008")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[4]], margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2011")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[5]], margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2019")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

#NBRI
a = stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/NBRI", pattern = ".tif$", full.names=TRUE,recursive=TRUE))
mapa = stack(a[[33]], a[[35]], a[[39]], a[[45]], a[[52]], a[[21]])
names(mapa) = c('2004', '2005', '2007', '2008', '2011', '2019')

#Crop rasters
mapa = crop(mapa, area1)

b = levelplot(mapa, margin = FALSE)
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[1]], margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2004")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[2]], margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2005")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[3]], margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2008")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[4]], margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2011")
b + layer(sp.lines(plots, lwd=2.0, col='black'))

b = levelplot(mapa[[5]], margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2019")
b + layer(sp.lines(plots, lwd=2.0, col='black'))
