################################################
# EXTRACT HYPERION VEGETATION INDEXS TO MATRIX #
#              CORRELATION                     #
#          => A Temporal scale <==             #
# By: Eduardo Q Marques   05-11-2019           #
################################################

#OBS: Same years are cutted control. So I change control area to other place that the forest is preserved
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)

#Data bank ===========================================================================
h04 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2004", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h05 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2005", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h06 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2006", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h08 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2008", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h10 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2010", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h11 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2011", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

#h12 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2012", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

#h13 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2013", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

#Polygon to get values
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Grid_Area1",layer="Grid_Area1_AA")
area1 = spTransform(area1, crs(h04))

area1df = as.data.frame(area1@data$ID)

area1df$transcto <- as.character(str_extract(area1df$`area1@data$ID`, "[A-Z, a-z]+"))
area1df$linha <- as.numeric(str_extract(area1df$`area1@data$ID`, "[0-9]+"))

for (x in 1:10) {
  area1df$parcela[area1df$linha == x] <- "controle"
}

for (x in 11:20) {
  area1df$parcela[area1df$linha == x] <- "b3yr"
}

for (x in 21:31) {
  area1df$parcela[area1df$linha == x] <- "b1yr"
}

area1df = area1df %>% 
  mutate(parc = parcela) %>% 
  mutate(transc = transcto) %>% 
  unite(col = "local", c("parc", "transc"), sep = '_')

colnames(area1df) = c("ID", "transcto", "linha", 'parcela', 'local')

area1@data <- area1df



area1 <- unionSpatialPolygons(area1, area1df$local)

plot(area1)

for (x in 1:63) {
  print(area1@polygons[[x]]@ID)
}










