###############################################
#          TEMPORAL SERIES PACKAGE            #
#          => A Temporal scale <==            #
# By: Eduardo Q Marques   10-02-2020          #
###############################################

#This script is for construction and test my oun package to work with temporal series and satelitte images

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)

#Functions =================
#To calculate median per plot
data_time_median = function(x,y){
  ext <- raster::extract(x, y)
  a <- melt(ext)
  a_md = a %>%
    group_by(date, parcela) %>% 
    summarise(index = median(index))
}

#Data bank to test=================
ndvi <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/NDVI", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

evi <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/EVI2", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

#Polygon to get values
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Hyperion",layer="Borda_nucleo_hyperion")
area1 = spTransform(area1, crs(ndvi))