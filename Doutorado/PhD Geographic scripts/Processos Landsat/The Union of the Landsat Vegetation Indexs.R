###############################################
# THE UNION OF THE LANDSAT VEGETATION INDEXS  #
#          => A Temporal scale <==            #
#            => CORE and EDGE <=              #
# By: Eduardo Q Marques   06-02-2020          #
###############################################

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)

#Data bank =================
ndvi <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/NDVI", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

evi <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/EVI2", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

vig <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/VIG", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

ndii <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/NDII", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

ndwi <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/NDWI", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

nbri <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/NBRI", pattern = ".tif$", full.names=TRUE,recursive=TRUE))


#Polygon to get values
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Hyperion",layer="Borda_nucleo_hyperion")
area1 = spTransform(area1, crs(ndvi))


#NDVI ======================
crt <- raster::extract(ndvi, area1[1,]); b3yr <- raster::extract(ndvi, area1[3,]); b1yr <- raster::extract(ndvi, area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi = as.data.frame(rbind(a, b, c))
ndvi1 <- ndvi[,c(2,3,5)]
colnames(ndvi1) = c("date", "ndvi", "parcela")


ndvi_md = ndvi1 %>%
  group_by(date, parcela) %>% 
  summarise(ndvi = median(ndvi))


#EVI =======================

#VIG =======================

#NDII ======================

#NDWI ======================

#NBRI ======================