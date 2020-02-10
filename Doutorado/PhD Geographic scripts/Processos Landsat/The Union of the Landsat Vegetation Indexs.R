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


#Functions =================
#To calculate median per plot
data_time_median = function(x,y,z,w){
  crt <- raster::extract(x, y)
  b3yr <- raster::extract(x, z)
  b1yr <- raster::extract(x, w)
  
  a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
  a = a %>% 
    mutate(parcela = "controle")
  b = b %>% 
    mutate(parcela = "b3yr")
  c = c %>% 
    mutate(parcela = "b1yr")
  index = as.data.frame(rbind(a, b, c))
  index1 <- index[,c(2,3,5)]
  colnames(index1) = c("date", "index", "parcela")
  index1$date = substr(index1$date, 18, 21)
  
  index_md = index1 %>%
    group_by(date, parcela) %>% 
    summarise(index = median(index))
  index_md
}


#NDVI ======================
ndvi_core = data_time_median(ndvi, area1[1,], area1[3,], area1[5,])

colnames(ndvi_core) = c("date", "ndvi", "parcela")

ggplot(ndvi_core, aes(date, ndvi, col=parcela))+
    geom_line(aes(group=parcela))

#EVI =======================
evi_core = data_time_median(evi, area1[1,], area1[3,], area1[5,])

colnames(evi_core) = c("date", "evi", "parcela")

ggplot(ndvi_core, aes(date, evi, col=parcela))+
  geom_line(aes(group=parcela))

#VIG =======================

#NDII ======================

#NDWI ======================

#NBRI ======================