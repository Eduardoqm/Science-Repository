library(raster)

#Listar as bandas (menos a banda 6)
list1 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band1.tif$", full.names=TRUE,recursive=TRUE)
list2 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band2.tif$", full.names=TRUE,recursive=TRUE)
list3 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band3.tif$", full.names=TRUE,recursive=TRUE)
list4 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band4.tif$", full.names=TRUE,recursive=TRUE)
list5 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band5.tif$", full.names=TRUE,recursive=TRUE)
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band7.tif$", full.names=TRUE,recursive=TRUE)

#convert data into rasterbrick for faster processing
b1 <- brick(stack(list1))
b2 <- brick(stack(list2))
b3 <- brick(stack(list3))
b4 <- brick(stack(list4))
b5 <- brick(stack(list5))
b7 <- brick(stack(list7))

#Enhanced Vegetation Index (EVI)
evi <- 2.5*((b4-b3)/(b4+6*b3-7.5*b1+1))

#Enhanced Vegetation Index - by two bands (EVI2)
evi2 <- 2.5*((b4-b3)/(b4+2.4*b3+1))

#Normalized Difference Vegetation Index (NDVI)
ndvi <- (b4-b3)/(b4+b3)

#Normalized Difference Glacier Index (NDGI) obs: Lenio (GRND)
ndgi <- (b2-b3)/(b2+b3)

#Normalized Difference Moisture Index (NDMI) obs: Lenio (MSI)
ndmi <- (b4-b5)/(b4+b5)

#SWND???
swnd <- (b5-b7)/(b5+b7)

#Normalized Burned Ratio Index (NBRI)
nbri <- (b4-b7)/(b4+b7)

#Normalized Pigment Chlorophyll Ratio Index (NPCRI)
npcri <- (b3-b1)/(b3+b1)





#Salve NDVI rasters
setwd("C:/Users/Eduardo Q Marques/Documents/NDVI")
for (i in 1:length(ndvi)) {
writeRaster(ndvi[[i]], names(b1[[i]]), format = "GTiff", bylayer = FALSE)
}
