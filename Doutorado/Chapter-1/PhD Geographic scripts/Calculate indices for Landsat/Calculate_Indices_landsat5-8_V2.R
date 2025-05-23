#Geracao de indices com imagens Landsat para a Area1 - Tang
#Eduardo Q Marques /30/07/2019/
#OBS: Update to calculate full Landsat-5 time series in 09-11-2020

library(raster)
#library(rasterVis)

#TM =============================================================================================
#List of satellite bands (menos a banda 6)
list1 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop5_1985-2011", pattern = "band1.tif$", full.names=TRUE,recursive=TRUE)
list2 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop5_1985-2011", pattern = "band2.tif$", full.names=TRUE,recursive=TRUE)
list3 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop5_1985-2011", pattern = "band3.tif$", full.names=TRUE,recursive=TRUE)
list4 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop5_1985-2011", pattern = "band4.tif$", full.names=TRUE,recursive=TRUE)
list5 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop5_1985-2011", pattern = "band5.tif$", full.names=TRUE,recursive=TRUE)
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop5_1985-2011", pattern = "band7.tif$", full.names=TRUE,recursive=TRUE)

#Convert data into rasterbrick for faster processing
b1 <- brick(stack(list1))
b2 <- brick(stack(list2))
b3 <- brick(stack(list3))
b4 <- brick(stack(list4))
b5 <- brick(stack(list5))
b7 <- brick(stack(list7))

#Normalize
b1 <- b1/10000
b2 <- b2/10000
b3 <- b3/10000
b4 <- b4/10000
b5 <- b5/10000
b7 <- b7/10000

#Calculate indices ------------------------------------------------------------------------------
#Enhanced Vegetation Index (EVI)
#evi <- 2.5*((b4-b3)/(b4+6*b3-7.5*b1+1))

#Enhanced Vegetation Index - by two bands (EVI2)
evi2 <- 2.5*((b4-b3)/(b4+2.4*b3+1))

#Normalized Difference Vegetation Index (NDVI)
ndvi <- (b4-b3)/(b4+b3)

#Green Red Normalized Difference (GRND) or Vegetation Indice of Greeness (VIG)
vig <- (b2-b3)/(b2+b3)

#Normalized Difference Infra-red (NDII) obs: the same formula (NBRI, NDWI, MSI)
ndii <- (b4-b5)/(b4+b5)

#SWND infrared index
swnd <- (b5-b7)/(b5+b7)

#Normalized Pigment Chlorophyll Ratio Index (NPCRI)
npcri <- (b3-b1)/(b3+b1)

#Salve Indices rasters--------------------------------------------------------------------------
#Names for indices
name1 = names(b1)
name1 = substr(name1, 18, 25)

name2 = c("evi2")
evi2_names = paste(name2, name1, sep = "-", collapse = NULL)

name2 = c("ndvi")
ndvi_names = paste(name2, name1, sep = "-", collapse = NULL)

name2 = c("vig")
vig_names = paste(name2, name1, sep = "-", collapse = NULL)

name2 = c("ndii")
ndii_names = paste(name2, name1, sep = "-", collapse = NULL)


#Save
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Landsat")

for (i in 1:length(evi2)) {
  writeRaster(evi2[[i]], evi2_names[[i]], format = "GTiff", bylayer = FALSE)
}

for (i in 1:length(ndvi)) {
  writeRaster(ndvi[[i]], ndvi_names[[i]], format = "GTiff", bylayer = FALSE)
}

for (i in 1:length(vig)) {
  writeRaster(vig[[i]], vig_names[[i]], format = "GTiff", bylayer = FALSE)
}

for (i in 1:length(ndii)) {
  writeRaster(ndii[[i]], ndii_names[[i]], format = "GTiff", bylayer = FALSE)
}


#OLI=============================================================================================
#List of satellite bands(banda 2 ate a banda 7 sao equivalentes as bandas do landsat5)
list2 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band2.tif$", full.names=TRUE,recursive=TRUE)
list3 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band3.tif$", full.names=TRUE,recursive=TRUE)
list4 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band4.tif$", full.names=TRUE,recursive=TRUE)
list5 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band5.tif$", full.names=TRUE,recursive=TRUE)
list6 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band6.tif$", full.names=TRUE,recursive=TRUE)
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Landsat/landsatcrop8", pattern = "band7.tif$", full.names=TRUE,recursive=TRUE)

#Convert data into rasterbrick for faster processing
b2 <- brick(stack(list2))
b3 <- brick(stack(list3))
b4 <- brick(stack(list4))
b5 <- brick(stack(list5))
b6 <- brick(stack(list6))
b7 <- brick(stack(list7))

#Normalize
b2 <- b2/10000
b3 <- b3/10000
b4 <- b4/10000
b5 <- b5/10000
b6 <- b6/10000
b7 <- b7/10000

#Calculate indices ==============================================================================
#Enhanced Vegetation Index (EVI)
#evi_b <- 2.5*((b5-b4)/(b5+6*b5-7.5*b2+1))

#Enhanced Vegetation Index - by two bands (EVI2)
evi2_b <- 2.5*((b5-b4)/(b5+2.4*b4+1))

#Normalized Difference Vegetation Index (NDVI)
ndvi_b <- (b5-b4)/(b5+b4)

#Green Red Normalized Difference (GRND) or Vegetation Indice of Greeness (VIG)
vig_b <- (b3-b4)/(b3+b4)

#Normalized Difference Infra-red (NDII) obs: the same formula (NBRI, NDWI, MSI)
ndii_b <- (b5-b6)/(b5+b6)

#SWND infrared index
swnd_b <- (b6-b7)/(b6+b7)

#Normalized Pigment Chlorophyll Ratio Index (NPCRI)
npcri_b <- (b4-b2)/(b4+b2)


#Salve Indices rasters--------------------------------------------------------------------------
#Names for indices
name1 = names(b2)
name1 = substr(name1, 18, 25)

name2 = c("evi2")
evi2_names = paste(name2, name1, sep = "-", collapse = NULL)

name2 = c("ndvi")
ndvi_names = paste(name2, name1, sep = "-", collapse = NULL)

name2 = c("vig")
vig_names = paste(name2, name1, sep = "-", collapse = NULL)

name2 = c("ndii")
ndii_names = paste(name2, name1, sep = "-", collapse = NULL)


#Save
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Landsat")

for (i in 1:length(evi2_b)) {
  writeRaster(evi2_b[[i]], evi2_names[[i]], format = "GTiff", bylayer = FALSE)
}

for (i in 1:length(ndvi_b)) {
  writeRaster(ndvi_b[[i]], ndvi_names[[i]], format = "GTiff", bylayer = FALSE)
}

for (i in 1:length(vig_b)) {
  writeRaster(vig_b[[i]], vig_names[[i]], format = "GTiff", bylayer = FALSE)
}

for (i in 1:length(ndii_b)) {
  writeRaster(ndii_b[[i]], ndii_names[[i]], format = "GTiff", bylayer = FALSE)
}

