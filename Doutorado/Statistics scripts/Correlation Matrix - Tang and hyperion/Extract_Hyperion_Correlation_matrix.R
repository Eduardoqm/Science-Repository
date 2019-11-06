################################################
# EXTRACT HYPERION VEGETATION INDEXS TO MATRIX #
#              CORRELATION                     #
#          => A Temporal scale <==             #
# By: Eduardo Q Marques   05-11-2019           #
################################################

#OBS: Same years are cutted control. So I change control area to other place that the forest is preserved
library(raster)
library(rgdal)
library(stringr)
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
  plot(h04[[6]])
  plot(area1[x,], color = 'blue', add = T)
}

crt_names = c( "controle_AA", "controle_B", "controle_C",  "controle_D",  "controle_E",  "controle_F",  "controle_G",  "controle_H",  "controle_I",  "controle_J",  "controle_K",  "controle_L",  "controle_M",  "controle_N",  "controle_O",  "controle_P",  "controle_Q",  "controle_R",  "controle_S",  "controle_T",  "controle_U")

b3yr_names = c( "b3yr_AA", "b3yr_B", "b3yr_C",  "b3yr_D",  "b3yr_E",  "b3yr_F",  "b3yr_G",  "b3yr_H",  "b3yr_I",  "b3yr_J",  "b3yr_K",  "b3yr_L",  "b3yr_M",  "b3yr_N",  "b3yr_O",  "b3yr_P",  "b3yr_Q",  "b3yr_R",  "b3yr_S",  "b3yr_T",  "b3yr_U")

b1yr_names = c( "b1yr_AA", "b1yr_B", "b1yr_C",  "b1yr_D",  "b1yr_E",  "b1yr_F",  "b1yr_G",  "b1yr_H",  "b1yr_I",  "b1yr_J",  "b1yr_K",  "b1yr_L",  "b1yr_M",  "b1yr_N",  "b1yr_O",  "b1yr_P",  "b1yr_Q",  "b1yr_R",  "b1yr_S",  "b1yr_T",  "b1yr_U")

#NDII ===================================================================
crt <- raster::extract(h04[[6]], area1[43:63,])
b3yr <- raster::extract(h04[[6]], area1[22:42,])
b1yr <- raster::extract(h04[[6]], area1[1:21,])

a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)

colnames(a) = c('index', 'transcto')
a = a %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = crt_names)


colnames(b) = c('index', 'transcto')
b = b %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b3yr_names)

colnames(c) = c('index', 'transcto')
c = c %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b1yr_names)


ndii1 = as.data.frame(rbind(a, b, c))
ndii1 <- ndii1[,c(2,3)]
colnames(ndii1) = c("ndii", "local")
ndii1 = ndii1 %>% 
  mutate(data = "2004")




crt <- raster::extract(h05[[6]], area1[43:63,])
b3yr <- raster::extract(h05[[6]], area1[22:42,])
b1yr <- raster::extract(h05[[6]], area1[1:21,])

a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)

colnames(a) = c('index', 'transcto')
a = a %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = crt_names)


colnames(b) = c('index', 'transcto')
b = b %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b3yr_names)

colnames(c) = c('index', 'transcto')
c = c %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b1yr_names)


ndii2 = as.data.frame(rbind(a, b, c))
ndii2 <- ndii2[,c(2,3)]
colnames(ndii2) = c("ndii", "local")
ndii2 = ndii2 %>% 
  mutate(data = "2005")




crt <- raster::extract(h06[[6]], area1[43:63,])
b3yr <- raster::extract(h06[[6]], area1[22:42,])
b1yr <- raster::extract(h06[[6]], area1[1:21,])

a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)

colnames(a) = c('index', 'transcto')
a = a %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = crt_names)


colnames(b) = c('index', 'transcto')
b = b %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b3yr_names)

colnames(c) = c('index', 'transcto')
c = c %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b1yr_names)


ndii3 = as.data.frame(rbind(a, b, c))
ndii3 <- ndii3[,c(2,3)]
colnames(ndii3) = c("ndii", "local")
ndii3 = ndii3 %>% 
  mutate(data = "2006")




crt <- raster::extract(h08[[6]], area1[43:63,])
b3yr <- raster::extract(h08[[6]], area1[22:42,])
b1yr <- raster::extract(h08[[6]], area1[1:21,])

a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)

colnames(a) = c('index', 'transcto')
a = a %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = crt_names)


colnames(b) = c('index', 'transcto')
b = b %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b3yr_names)

colnames(c) = c('index', 'transcto')
c = c %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b1yr_names)


ndii4 = as.data.frame(rbind(a, b, c))
ndii4 <- ndii4[,c(2,3)]
colnames(ndii4) = c("ndii", "local")
ndii4 = ndii4 %>% 
  mutate(data = "2008")





crt <- raster::extract(h10[[6]], area1[43:63,])
b3yr <- raster::extract(h10[[6]], area1[22:42,])
b1yr <- raster::extract(h10[[6]], area1[1:21,])

a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)

colnames(a) = c('index', 'transcto')
a = a %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = crt_names)


colnames(b) = c('index', 'transcto')
b = b %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b3yr_names)

colnames(c) = c('index', 'transcto')
c = c %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b1yr_names)


ndii5 = as.data.frame(rbind(a, b, c))
ndii5 <- ndii5[,c(2,3)]
colnames(ndii5) = c("ndii", "local")
ndii5 = ndii5 %>% 
  mutate(data = "2010")





crt <- raster::extract(h11[[6]], area1[43:63,])
b3yr <- raster::extract(h11[[6]], area1[22:42,])
b1yr <- raster::extract(h11[[6]], area1[1:21,])

a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)

colnames(a) = c('index', 'transcto')
a = a %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = crt_names)


colnames(b) = c('index', 'transcto')
b = b %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b3yr_names)

colnames(c) = c('index', 'transcto')
c = c %>% 
  group_by(transcto) %>% 
  summarise(index = median(index)) %>% 
  mutate(local = b1yr_names)


ndii6 = as.data.frame(rbind(a, b, c))
ndii6 <- ndii6[,c(2,3)]
colnames(ndii6) = c("ndii", "local")
ndii6 = ndii6 %>% 
  mutate(data = "2011")


ndii = as.data.frame(rbind(ndii1, ndii2, ndii3, ndii4, ndii5, ndii6))

