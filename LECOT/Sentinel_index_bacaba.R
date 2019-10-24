#Vegetation index to Bacaba Park from Sentinel-2
#Eduardo Q Marques /11/10/2019/

library(raster)
library(rasterVis)
library(rgdal)

#Data bank
setwd("C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\EQMapas\\Pratica Divino\\Sentinel_bacaba")

b3 = raster("S2A_OPER_MSI_L1C_TL_MTI__20160725T200552_A005698_T22LCJ_B03.jp2")
b4 = raster("S2A_OPER_MSI_L1C_TL_MTI__20160725T200552_A005698_T22LCJ_B04.jp2")
b8 = raster("S2A_OPER_MSI_L1C_TL_MTI__20160725T200552_A005698_T22LCJ_B08.jp2")
b12 = raster("S2A_OPER_MSI_L1C_TL_MTI__20160725T200552_A005698_T22LCJ_B12.jp2")

bcb = readOGR(dsn = "C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\EQMapas\\shapes",layer="limite_reserva_total")

#Cutting satellite image
bcb = spTransform(bcb, crs(b3))
b3 = crop(b3, bcb)
b4 = crop(b4, bcb)
b8 = crop(b8, bcb)
b12 = crop(b12, bcb)

#Solve difference in pixel resolution
b12 <- projectRaster(b12,b8,method = 'ngb')

#Index--------------------
#Normalized Difference Vegetation Index (NDVI)
ndvi <- (b8-b4)/(b8+b4)

#Normalized Burned Ratio Index (NBRI)
nbri <- (b8-b12)/(b8+b12)

#Normalized Difference Water Index (NDWI)
ndwi <- (b3-b8)/(b3+b8)

#Salve rasters------
setwd("C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\EQMapas\\Pratica Divino\\Sentinel_bacaba")
writeRaster(ndvi, filename = "ndvi_bcb.tif", format = "GTiff")
writeRaster(ndwi, filename = "ndwi_bcb.tif", format = "GTiff")
writeRaster(nbri, filename = "nbri_bcb.tif", format = "GTiff")

#Make maps
#NDVI
ndvi = mask(ndvi, bcb)
levelplot(ndvi, margin = FALSE, par.settings = RdBuTheme, main = "NDVI 2016")
#layer(sp.lines(bcb, lwd=2.0, col='black'))

#NDWI
ndwi = mask(ndwi, bcb)
levelplot(ndwi, margin = FALSE, par.settings = RdBuTheme, main = "NDWI 2016")

#NBRI
nbri = mask(nbri, bcb)
levelplot(nbri, margin = FALSE, par.settings = RdBuTheme, main = "NBRI 2016")















