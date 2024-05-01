
library(raster)
library(rasterVis)
library(rgdal)
library(viridis)

setwd("C:/Users/Eduardo Q Marques/Downloads/Imagens de drone tanguro/imagens para testes")
dir()

img = brick(choose.files())
#b3 = readOGR("Parcela_B.shp")


#plotRGB(img, r=1, g=2, b=3)
plot(img, col = viridis(100))
levelplot(img, margin = F, main = "Not calibrated jpeg Soja",
          col.regions = viridis(100))
#img2 = crop(img, b3)


ndvi <- (img[[3]] - img[[1]]) / (img[[3]] + img[[1]])

#plot(ndvi, col = viridis(100), main = "NDVI not calibrated Soja")
levelplot(ndvi, margin = F, main = "NDVI not calibrated Soja",
          col.regions = viridis(100))


#writeRaster(ndvi, "B3yr_NDVI_15_11_2018.tif", format="GTiff")
#writeRaster(img2, "B3yr_15_11_2018.tif", format="GTiff")