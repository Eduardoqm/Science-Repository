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


#Calculate edge indexes ===================
#NDVI
ndvi_edge = data_time_median(ndvi, area1[1,], area1[3,], area1[5,])

colnames(ndvi_edge) = c("date", "parcela", "ndvi")

ggplot(ndvi_edge, aes(date, ndvi, col=parcela))+
  geom_line(aes(group=parcela))

#EVI
evi_edge = data_time_median(evi, area1[1,], area1[3,], area1[5,])

colnames(evi_edge) = c("date", "parcela", "evi")

ggplot(evi_edge, aes(date, evi, col=parcela))+
  geom_line(aes(group=parcela))

#VIG
vig_edge = data_time_median(vig, area1[1,], area1[3,], area1[5,])

colnames(vig_edge) = c("date", "parcela", "vig")

ggplot(vig_edge, aes(date, vig, col=parcela))+
  geom_line(aes(group=parcela))

#NDII
ndii_edge = data_time_median(ndii, area1[1,], area1[3,], area1[5,])

colnames(ndii_edge) = c("date", "parcela", "ndii")

ggplot(ndii_edge, aes(date, ndii, col=parcela))+
  geom_line(aes(group=parcela))

#NDWI
ndwi_edge = data_time_median(ndwi, area1[1,], area1[3,], area1[5,])

colnames(ndwi_edge) = c("date", "parcela", "ndwi")

ggplot(ndwi_edge, aes(date, ndwi, col=parcela))+
  geom_line(aes(group=parcela))

#NBRI
nbri_edge = data_time_median(nbri, area1[1,], area1[3,], area1[5,])

colnames(nbri_edge) = c("date", "parcela", "nbri")

ggplot(nbri_edge, aes(date, nbri, col=parcela))+
  geom_line(aes(group=parcela))

#Calculate core indexes
#NDVI
ndvi_core = data_time_median(ndvi, area1[2,], area1[4,], area1[6,])

colnames(ndvi_core) = c("date", "parcela", "ndvi")

ggplot(ndvi_core, aes(date, ndvi, col=parcela))+
  geom_line(aes(group=parcela))

#EVI
evi_core = data_time_median(evi, area1[2,], area1[4,], area1[6,])

colnames(evi_core) = c("date", "parcela", "evi")

ggplot(evi_core, aes(date, evi, col=parcela))+
  geom_line(aes(group=parcela))

#VIG
vig_core = data_time_median(vig, area1[2,], area1[4,], area1[6,])

colnames(vig_core) = c("date", "parcela", "vig")

ggplot(vig_core, aes(date, vig, col=parcela))+
  geom_line(aes(group=parcela))

#NDII
ndii_core = data_time_median(ndii, area1[2,], area1[4,], area1[6,])

colnames(ndii_core) = c("date", "parcela", "ndii")

ggplot(ndii_core, aes(date, ndii, col=parcela))+
  geom_line(aes(group=parcela))

#NDWI
ndwi_core = data_time_median(ndwi, area1[2,], area1[4,], area1[6,])

colnames(ndwi_core) = c("date", "parcela", "ndwi")

ggplot(ndwi_core, aes(date, ndwi, col=parcela))+
  geom_line(aes(group=parcela))

#NBRI
nbri_core = data_time_median(nbri, area1[2,], area1[4,], area1[6,])

colnames(nbri_core) = c("date", "parcela", "nbri")

ggplot(nbri_core, aes(date, nbri, col=parcela))+
  geom_line(aes(group=parcela))

#Union of edge and core indexes =====
edge = cbind(ndvi_edge, evi_edge, vig_edge, ndii_edge, ndwi_edge, nbri_edge)
edge = edge[,c(1,2,3,6,9,12,15,18)]
edge$dist = c("borda")

core = cbind(ndvi_core, evi_core, vig_core, ndii_core, ndwi_core, nbri_core)
core = core[,c(1,2,3,6,9,12,15,18)]
core$dist = c("nucleo")

landsat_all = rbind(edge, core)

#Write data frame
#setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")
#write.csv(landsat_all, file = "Landast_indexs_median by plot.csv", sep = ",")








#With all data ================================================================================
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
  index1 <- index[,c(2,5,3)]
  colnames(index1) = c("date", "parcela", "index")
  index1$date = substr(index1$date, 18, 21)
  index2 = as.data.frame(index1)
  #index_md = index1 %>%
  #  group_by(date, parcela) %>% 
  #  summarise(index = median(index))
  #index_md
}


#Calculate edge indexes ===================
#NDVI
ndvi_edge = data_time_median(ndvi, area1[1,], area1[3,], area1[5,])

colnames(ndvi_edge) = c("date", "parcela", "ndvi")

ggplot(ndvi_edge, aes(date, ndvi, fill=parcela))+
  geom_boxplot()

#EVI
evi_edge = data_time_median(evi, area1[1,], area1[3,], area1[5,])

colnames(evi_edge) = c("date", "parcela", "evi")

ggplot(evi_edge, aes(date, evi, fill=parcela))+
  geom_boxplot()

#VIG
vig_edge = data_time_median(vig, area1[1,], area1[3,], area1[5,])

colnames(vig_edge) = c("date", "parcela", "vig")

ggplot(vig_edge, aes(date, vig, fill=parcela))+
  geom_boxplot()

#NDII
ndii_edge = data_time_median(ndii, area1[1,], area1[3,], area1[5,])

colnames(ndii_edge) = c("date", "parcela", "ndii")

ggplot(ndii_edge, aes(date, ndii, fill=parcela))+
  geom_boxplot()

#NDWI
ndwi_edge = data_time_median(ndwi, area1[1,], area1[3,], area1[5,])

colnames(ndwi_edge) = c("date", "parcela", "ndwi")

ggplot(ndwi_edge, aes(date, ndwi, fill=parcela))+
  geom_boxplot()

#NBRI
nbri_edge = data_time_median(nbri, area1[1,], area1[3,], area1[5,])

colnames(nbri_edge) = c("date", "parcela", "nbri")

ggplot(nbri_edge, aes(date, nbri, fill=parcela))+
  geom_boxplot()

#Calculate core indexes
#NDVI
ndvi_core = data_time_median(ndvi, area1[2,], area1[4,], area1[6,])

colnames(ndvi_core) = c("date", "parcela", "ndvi")

ggplot(ndvi_core, aes(date, ndvi, fill=parcela))+
  geom_boxplot()

#EVI
evi_core = data_time_median(evi, area1[2,], area1[4,], area1[6,])

colnames(evi_core) = c("date", "parcela", "evi")

ggplot(evi_core, aes(date, evi, fill=parcela))+
  geom_boxplot()

#VIG
vig_core = data_time_median(vig, area1[2,], area1[4,], area1[6,])

colnames(vig_core) = c("date", "parcela", "vig")

ggplot(vig_core, aes(date, vig, fill=parcela))+
  geom_boxplot()

#NDII
ndii_core = data_time_median(ndii, area1[2,], area1[4,], area1[6,])

colnames(ndii_core) = c("date", "parcela", "ndii")

ggplot(ndii_core, aes(date, ndii, fill=parcela))+
  geom_boxplot()

#NDWI
ndwi_core = data_time_median(ndwi, area1[2,], area1[4,], area1[6,])

colnames(ndwi_core) = c("date", "parcela", "ndwi")

ggplot(ndwi_core, aes(date, ndwi, fill=parcela))+
  geom_boxplot()

#NBRI
nbri_core = data_time_median(nbri, area1[2,], area1[4,], area1[6,])

colnames(nbri_core) = c("date", "parcela", "nbri")

ggplot(nbri_core, aes(date, nbri, fill=parcela))+
  geom_boxplot()

#Union of edge and core indexes =====
edge = cbind(ndvi_edge, evi_edge, vig_edge, ndii_edge, ndwi_edge, nbri_edge)
edge = edge[,c(1,2,3,6,9,12,15,18)]
edge$dist = c("borda")

core = cbind(ndvi_core, evi_core, vig_core, ndii_core, ndwi_core, nbri_core)
core = core[,c(1,2,3,6,9,12,15,18)]
core$dist = c("nucleo")

landsat_all = rbind(edge, core)

#Write data frame
#setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")


#write.csv(landsat_all, file = "Landast_indexs_all by plot.csv", sep = ",")
