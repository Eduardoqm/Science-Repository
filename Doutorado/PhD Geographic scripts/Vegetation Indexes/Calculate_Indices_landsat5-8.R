#Geracao de indices com imagens Landsat para a Area1 - Tang
#Eduardo Q Marques /30/07/2019/

library(raster)
library(rasterVis)

#TM-------------------------------------------------------------------------------------------------------------------
#List of satellite bands (menos a banda 6)
list1 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop5", pattern = "band1.tif$", full.names=TRUE,recursive=TRUE)
list2 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop5", pattern = "band2.tif$", full.names=TRUE,recursive=TRUE)
list3 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop5", pattern = "band3.tif$", full.names=TRUE,recursive=TRUE)
list4 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop5", pattern = "band4.tif$", full.names=TRUE,recursive=TRUE)
list5 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop5", pattern = "band5.tif$", full.names=TRUE,recursive=TRUE)
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop5", pattern = "band7.tif$", full.names=TRUE,recursive=TRUE)

#Convert data into rasterbrick for faster processing
b1 <- brick(stack(list1))
b2 <- brick(stack(list2))
b3 <- brick(stack(list3))
b4 <- brick(stack(list4))
b5 <- brick(stack(list5))
b7 <- brick(stack(list7))

#Index-------------------------------------------------------------------------------------------------------------
#Enhanced Vegetation Index (EVI)
evi <- 2.5*((b4-b3)/(b4+6*b3-7.5*b1+1))

#Enhanced Vegetation Index - by two bands (EVI2)
evi2 <- 2.5*((b4-b3)/(b4+2.4*b3+1))

#Normalized Difference Vegetation Index (NDVI)
ndvi <- (b4-b3)/(b4+b3)

#Green Red Normalized Difference (GRND)
grnd <- (b2-b3)/(b2+b3)

#Normalized Difference Moisture Index (NDMI) obs: Lenio (MSI)
ndmi <- (b4-b5)/(b4+b5)

#SWND infrared index
swnd <- (b5-b7)/(b5+b7)

#Normalized Burned Ratio Index (NBRI)
nbri <- (b4-b7)/(b4+b7)

#Normalized Pigment Chlorophyll Ratio Index (NPCRI)
npcri <- (b3-b1)/(b3+b1)

#Normalized Difference Water Index (NDWI)
ndwi <- (b4-b5)/(b4+b5)

#Salve NDVI rasters----------------------------------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/EVI")
for (i in 1:length(evi)) {
  writeRaster(evi[[i]], names(b1[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/EVI2")
for (i in 1:length(evi2)) {
  writeRaster(evi2[[i]], names(b1[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/NDVI")
for (i in 1:length(ndvi)) {
  writeRaster(ndvi[[i]], names(b1[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/GRND")
for (i in 1:length(grnd)) {
  writeRaster(grnd[[i]], names(b1[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/NDMI")
for (i in 1:length(ndmi)) {
  writeRaster(ndmi[[i]], names(b1[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/SWND")
for (i in 1:length(swnd)) {
  writeRaster(swnd[[i]], names(b1[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/NBRI")
for (i in 1:length(nbri)) {
  writeRaster(nbri[[i]], names(b1[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/NPCRI")
for (i in 1:length(npcri)) {
  writeRaster(npcri[[i]], names(b1[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/NDWI")
for (i in 1:length(ndwi)) {
  writeRaster(ndwi[[i]], names(b1[[i]]), format = "GTiff", bylayer = FALSE)
}

#OLI------------------------------------------------------------------------------------------------------------------
#List of satellite bands(banda 2 ate a banda 7 são equivalentes as bandas do landsat5)
list2 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band2.tif$", full.names=TRUE,recursive=TRUE)
list3 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band3.tif$", full.names=TRUE,recursive=TRUE)
list4 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band4.tif$", full.names=TRUE,recursive=TRUE)
list5 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band5.tif$", full.names=TRUE,recursive=TRUE)
list6 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band6.tif$", full.names=TRUE,recursive=TRUE)
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band7.tif$", full.names=TRUE,recursive=TRUE)

#Convert data into rasterbrick for faster processing
b2 <- brick(stack(list2))
b3 <- brick(stack(list3))
b4 <- brick(stack(list4))
b5 <- brick(stack(list5))
b6 <- brick(stack(list6))
b7 <- brick(stack(list7))

#Index---------------------------------------------------------------------------------------------------------------
#Enhanced Vegetation Index (EVI)
evi_b <- 2.5*((b5-b4)/(b5+6*b5-7.5*b2+1))

#Enhanced Vegetation Index - by two bands (EVI2)
evi2_b <- 2.5*((b5-b4)/(b5+2.4*b4+1))

#Normalized Difference Vegetation Index (NDVI)
ndvi_b <- (b5-b4)/(b5+b4)

#Normalized Difference Glacier Index (NDGI) obs: Lenio (GRND)
grnd_b <- (b3-b4)/(b3+b4)

#Normalized Difference Moisture Index (NDMI) obs: Lenio (MSI)
ndmi_b <- (b5-b6)/(b5+b6)

#SWND???
swnd_b <- (b6-b7)/(b6+b7)

#Normalized Burned Ratio Index (NBRI)
nbri_b <- (b5-b7)/(b5+b7)

#Normalized Pigment Chlorophyll Ratio Index (NPCRI)
npcri_b <- (b4-b2)/(b4+b2)

#Normalized Difference Water Index (NDWI)
ndwi_b <- (b5-b6)/(b5+b6)

#Salve NDVI rasters-----------------------------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/EVI")
for (i in 1:length(evi_b)) {
  writeRaster(evi_b[[i]], names(b2[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/EVI2")
for (i in 1:length(evi2_b)) {
  writeRaster(evi2_b[[i]], names(b2[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/NDVI")
for (i in 1:length(ndvi_b)) {
  writeRaster(ndvi_b[[i]], names(b2[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/GRND")
for (i in 1:length(grnd_b)) {
  writeRaster(grnd_b[[i]], names(b2[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/NDMI")
for (i in 1:length(ndmi_b)) {
  writeRaster(ndmi_b[[i]], names(b2[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/SWND")
for (i in 1:length(swnd_b)) {
  writeRaster(swnd_b[[i]], names(b2[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/NBRI")
for (i in 1:length(nbri_b)) {
  writeRaster(nbri_b[[i]], names(b2[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/NPCRI")
for (i in 1:length(npcri_b)) {
  writeRaster(npcri_b[[i]], names(b2[[i]]), format = "GTiff", bylayer = FALSE)
}
setwd("C:/Users/Eduardo Q Marques/Documents/NDWI")
for (i in 1:length(ndwi_b)) {
  writeRaster(ndwi_b[[i]], names(b2[[i]]), format = "GTiff", bylayer = FALSE)
}
