############################################################
# Extrair dados de solos do IBGE por Municipio do MATOPIBA #
############################################################
# carregar library
library(ggmap)
library(raster)
library(maptools)
library(spatial.tools)
library(snow)
library(rgdal)
require(XLConnect)
library(ipam)
library(ggplot2)
library(dplyr)
library(rgeos)
library("stringr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

# Diretorio do shape de municipios
setwd("/home/eduardo/Documents/public-ipam/data_geo/shapes/municipios_IBGE/municipios_dissolve/")
dir()
proj2=CRS("+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
mun2001 <- readShapePoly("Mtopiba_2001.shp",verbose=TRUE, proj4string=proj2)
P4S <- CRS("+proj=longlat +datum=WGS84")

##
string_fun <- function(x) {
  ul = unlist(strsplit(x, split = "\\s+"))[1:10]
  paste(ul,collapse=" ")
}
##
# Diretorio do shape de solos
setwd("/home/eduardo/Documents/public-ipam/data_geo/shapes/Solos do Brasil/")
dir()
solos <- readShapePoly("Solos_5000.shp",verbose=TRUE, proj4string=P4S)
solos@data$main_class=stringr::word(string = as.character(solos@data$DSC_COMPON),
                                    start = 1, end = 1, sep = fixed(" "))
head(solos)  
unique(solos@data$main_class)


#Criar e salvar o shape
writeOGR(solos, 
         dsn = '/home/eduardo/Documents/public-ipam/data_geo/Base_raster/Dados Spera/Mato Grosso (Cerrado)/only_tif',
         layer ='solos_ibge', driver = 'ESRI Shapefile',overwrite=TRUE)

##############################
# Manipulando arquivo RASTER #
##############################

## OBS: Transformei o shape em raster no ArcGis ##

#Abrir raster dos solos
setwd("/home/eduardo/Documents/public-ipam/data_geo/Base_raster/Solos do Brasil/")
dir()
solos_raster<- raster("solos_ibge_raster2.tif",verbose=TRUE, proj4string=proj2)
plot(solos_raster)
head(solos_raster)

#Recortar solos para os municipios do MATOPIBA
P4S<-CRS("+proj=longlat +datum=WGS84")
mun2001b <- spTransform(mun2001, P4S)
mun2001@data$solos<-extract(solos_raster,mun2001,sp=TRUE,method="mod", na.rm = TRUE,factor=TRUE)
plot(mun2001)

#solo_mun2<-raster::extract(solos_raster,mun2001b,sp=TRUE,method="simple", na.rm = TRUE)
#solo_mun<-extract(solos_raster, mun2001b, weights = TRUE, small = TRUE)
#<- raster::extract(solos_raster, mun2001, fun = sum, na.rm = TRUE,sp=FALSE)
#head(solo_mun)
#summary(solo_mun)

writeRaster(solo_mun3, "Solo_IBGE", format = "GTiff")

writeOGR(mun2001, 
         dsn = '/home/eduardo/Documents/public-ipam/data_geo/Base_raster/Dados Spera/Mato Grosso (Cerrado)/only_tif',
         layer ='solos_shape_mun', driver = 'ESRI Shapefile',overwrite=TRUE)

