#Vegetation index to Bacaba Park from Landsat 8
#Eduardo Q Marques /11/10/2019/

library(raster)
library(rasterVis)
library(rgdal)
library(gridExtra)

#Before fire =========
#Data bank
setwd("C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\EQMapas\\Pratica Divino\\Landsat_bacaba\\2019-09\\Before")

b4 = raster("20190829.B4.tif")
b5 = raster("20190829.B5.tif")
b6 = raster("20190829.B6.tif")
b7 = raster("20190829.B7.tif")

bcb = readOGR(dsn = "C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\EQMapas\\shapes",layer="limite_reserva_total")

#Cutting satellite image
bcb = spTransform(bcb, crs(b4))
b4 = crop(b4, bcb)
b5 = crop(b5, bcb)
b6 = crop(b6, bcb)
b7 = crop(b7, bcb)

#Index--------------------
#Normalized Difference Vegetation Index (NDVI)
ndvi <- (b5-b4)/(b5+b4)

#Normalized Burned Ratio Index (NBRI)
nbri <- (b5-b7)/(b5+b7)

#Normalized Difference Water Index (NDWI)
ndwi <- (b5-b6)/(b5+b6)

#Salve rasters------
#setwd("C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\EQMapas\\Pratica Divino\\Landsat_bacaba\\2015-06")
#writeRaster(ndvi, filename = "ndvi_bcb.tif", format = "GTiff")
#writeRaster(ndwi, filename = "ndwi_bcb.tif", format = "GTiff")
#writeRaster(nbri, filename = "nbri_bcb.tif", format = "GTiff")

#Make maps
#NDVI
ndvi = mask(ndvi, bcb)
levelplot(ndvi, margin = FALSE, par.settings = RdBuTheme, main = "NDVI 2019 - before fire")
#layer(sp.lines(bcb, lwd=2.0, col='black'))

#NDWI
ndwi = mask(ndwi, bcb)
levelplot(ndwi, margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2019 - before fire")

#NBRI
nbri = mask(nbri, bcb)
levelplot(nbri, margin = FALSE, par.settings = RdBuTheme, main = "NBRI 2019 - before fire")

#After fire ==========
#Data bank
setwd("C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\EQMapas\\Pratica Divino\\Landsat_bacaba\\2019-09\\After")

b4 = raster("20190914.B4.tif")
b5 = raster("20190914.B5.tif")
b6 = raster("20190914.B6.tif")
b7 = raster("20190914.B7.tif")

#Cutting satellite image
b4 = crop(b4, bcb)
b5 = crop(b5, bcb)
b6 = crop(b6, bcb)
b7 = crop(b7, bcb)

#Index--------------------
#Normalized Difference Vegetation Index (NDVI)
ndvi_b <- (b5-b4)/(b5+b4)

#Normalized Burned Ratio Index (NBRI)
nbri_b <- (b5-b7)/(b5+b7)

#Normalized Difference Water Index (NDWI)
ndwi_b <- (b5-b6)/(b5+b6)

#Salve rasters------
#setwd("C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\EQMapas\\Pratica Divino\\Landsat_bacaba\\2015-09")
#writeRaster(ndvi, filename = "ndvi_bcb.tif", format = "GTiff")
#writeRaster(ndwi, filename = "ndwi_bcb.tif", format = "GTiff")
#writeRaster(nbri, filename = "nbri_bcb.tif", format = "GTiff")

#Make maps
#NDVI
ndvi_b = mask(ndvi_b, bcb)
levelplot(ndvi_b, margin = FALSE, par.settings = RdBuTheme, main = "NDVI 2019 - after fire")
#layer(sp.lines(bcb, lwd=2.0, col='black'))

#NDWI
ndwi_b = mask(ndwi_b, bcb)
levelplot(ndwi_b, margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2019 - after fire")

#NBRI
nbri_b = mask(nbri_b, bcb)
levelplot(nbri_b, margin = FALSE, par.settings = RdBuTheme, main = "NBRI 2019 - after fire")

#Comparative =======
nbri_b <- projectRaster(nbri_b, nbri, method = 'ngb')
nbri_all = stack(nbri, nbri_b)

ndvi_b <- projectRaster(ndvi_b, ndvi, method = 'ngb')
ndvi_all = stack(ndvi, ndvi_b)

ndwi_b <- projectRaster(ndwi_b, ndwi, method = 'ngb')
ndwi_all = stack(ndwi, ndwi_b)

levelplot(nbri_all, margin = FALSE, par.settings = RdBuTheme, main = "NBRI 2019 - before and after fire")

levelplot(ndvi_all, margin = FALSE, par.settings = RdBuTheme, main = "NDVI 2019 - before and after fire")

levelplot(ndwi_all, margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2019 - before and after fire")




#Vegetation index to Bacaba Park from Landsat 8
#Eduardo Q Marques /11/10/2019/

library(raster)
library(rasterVis)
library(rgdal)
library(gridExtra)
library(viridis)
library(ggplot2)


bcb = readOGR(dsn = "C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\EQMapas\\Pratica Divino\\Landsat_bacaba",layer="Curso_campo")



#After fire ==========
#Data bank
setwd("C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\EQMapas\\Pratica Divino\\Landsat_bacaba\\2019-09\\After")

b4 = raster("20190914.B4.tif")
b5 = raster("20190914.B5.tif")
b6 = raster("20190914.B6.tif")
b7 = raster("20190914.B7.tif")

#Cutting satellite image
bcb = spTransform(bcb, crs(b4))
b4 = crop(b4, bcb)
b5 = crop(b5, bcb)
b6 = crop(b6, bcb)
b7 = crop(b7, bcb)

#Index--------------------
#Normalized Difference Vegetation Index (NDVI)
ndvi_b <- (b5-b4)/(b5+b4)

#Make maps
#NDVI
ndvi_b = mask(ndvi_b, bcb)
levelplot(ndvi_b, margin = FALSE, colours = viridis(10), main = "NDVI 2019 - after fire")
#layer(sp.lines(bcb, lwd=2.0, col='black'))


point = rasterToPoints(ndvi_b)
df = data.frame(point)

ggplot(df, aes(x,y)) +
  geom_raster(aes(fill = layer)) +
  scale_fill_gradientn("NDVI", colours = viridis(10)) +
  theme_minimal()+
  labs(title = "Fogo no Bacaba - 2019", x = "", y = "")

