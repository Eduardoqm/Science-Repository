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
library(GGally)

#Data bank ===========================================================================
h04 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2004", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h05 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2005", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h06 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2006", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h08 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2008", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h10 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2010", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h11 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2011", pattern = ".tif$", full.names=TRUE,recursive=TRUE))


#Polygon to get values ==================================
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Grid_Area1",layer="Grid_Area1_AA_hyperion")
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


#LWVI2 ========================================================
crt <- raster::extract(h04[[4]], area1[43:63,])
b3yr <- raster::extract(h04[[4]], area1[22:42,])
b1yr <- raster::extract(h04[[4]], area1[1:21,])

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


lwvi21 = as.data.frame(rbind(a, b, c))
lwvi21 <- lwvi21[,c(2,3)]
colnames(lwvi21) = c("lwvi2", "local")
lwvi21 = lwvi21 %>% 
  mutate(data = "2004")




crt <- raster::extract(h05[[4]], area1[43:63,])
b3yr <- raster::extract(h05[[4]], area1[22:42,])
b1yr <- raster::extract(h05[[4]], area1[1:21,])

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


lwvi22 = as.data.frame(rbind(a, b, c))
lwvi22 <- lwvi22[,c(2,3)]
colnames(lwvi22) = c("lwvi2", "local")
lwvi22 = lwvi22 %>% 
  mutate(data = "2005")




crt <- raster::extract(h06[[4]], area1[43:63,])
b3yr <- raster::extract(h06[[4]], area1[22:42,])
b1yr <- raster::extract(h06[[4]], area1[1:21,])

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


lwvi23 = as.data.frame(rbind(a, b, c))
lwvi23 <- lwvi23[,c(2,3)]
colnames(lwvi23) = c("lwvi2", "local")
lwvi23 = lwvi23 %>% 
  mutate(data = "2006")




crt <- raster::extract(h08[[4]], area1[43:63,])
b3yr <- raster::extract(h08[[4]], area1[22:42,])
b1yr <- raster::extract(h08[[4]], area1[1:21,])

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


lwvi24 = as.data.frame(rbind(a, b, c))
lwvi24 <- lwvi24[,c(2,3)]
colnames(lwvi24) = c("lwvi2", "local")
lwvi24 = lwvi24 %>% 
  mutate(data = "2008")





crt <- raster::extract(h10[[4]], area1[43:63,])
b3yr <- raster::extract(h10[[4]], area1[22:42,])
b1yr <- raster::extract(h10[[4]], area1[1:21,])

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


lwvi25 = as.data.frame(rbind(a, b, c))
lwvi25 <- lwvi25[,c(2,3)]
colnames(lwvi25) = c("lwvi2", "local")
lwvi25 = lwvi25 %>% 
  mutate(data = "2010")





crt <- raster::extract(h11[[4]], area1[43:63,])
b3yr <- raster::extract(h11[[4]], area1[22:42,])
b1yr <- raster::extract(h11[[4]], area1[1:21,])

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


lwvi26 = as.data.frame(rbind(a, b, c))
lwvi26 <- lwvi26[,c(2,3)]
colnames(lwvi26) = c("lwvi2", "local")
lwvi26 = lwvi26 %>% 
  mutate(data = "2011")


lwvi2 = as.data.frame(rbind(lwvi21, lwvi22, lwvi23, lwvi24, lwvi25, lwvi26))

#Union of indexs ============================================
index = cbind(ndii, lwvi2)
index = index[,c(1,4,5,6)]



setwd('C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\Doutorado\\Deposito\\Banco de Dados Tanguro\\Dados para analise cap1')

#Plot data organization
#Biomass ====================================

#Transformar linha em transceto!!!! <====


biomass = read.csv("Biomassa_Tang.csv", sep = ",", header = TRUE)
biomass = biomass[,c(1:7)]
colnames(biomass) = c('plot', 'transcto', '2004', '2008', '2010', '2011', '2012')
biomass = melt(biomass)
colnames(biomass) = c('plot', 'transcto', 'data', 'biomass')


# BEM AQUI!!! <== #Transformar linha em transceto e AA, A, AB em AA!!!!


biomass = biomass %>% 
  na.omit() %>% 
  group_by(plot, transcto, data) %>% 
  summarise(biomass = sum(biomass))%>%
  ungroup()%>%
  mutate(parcela=factor(plot,labels=c("controle","b3yr","b1yr")))

biomass = biomass[,-1]

biomass = biomass %>% 
  mutate(parc = parcela) %>% 
  mutate(transc = transcto) %>% 
  unite(col = "local", c("parc", "transc"), sep = '_')


#Fuel ==================================
fuel = read.csv("Combustivel_Brown_Tang.csv", sep = ",", header = TRUE)

#Transformar borda, BORDA, Borda, AA, A e AB em AA!!!!

fuel = fuel %>% 
  na.omit() %>% 
  mutate(fuel = NI06 + NI25 + NI76) 

fuel = fuel[,c(1,2,7)]

#Extract numbers only
fuel$transcto <- as.character(str_extract(fuel$ponto, "[A-Z, a-z]+"))
fuel$ponto <- as.numeric(str_extract(fuel$ponto, "[0-9]+"))
fuel = na.omit(fuel)
colnames(fuel) = c("parcela", "data", "fuel", "transcto")

for (x in 1:10) {
  fuel$parcela[fuel$parcela == x] <- "controle"
}

for (x in 11:20) {
  fuel$parcela[fuel$parcela == x] <- "b3yr"
}

for (x in 21:31) {
  fuel$parcela[fuel$parcela == x] <- "b1yr"
}

fuel = fuel %>% 
  group_by(parcela, transcto, data) %>% 
  summarise(fuel = median(fuel)) %>% 
  mutate(parc = parcela) %>% 
  mutate(transc = transcto) %>% 
  unite(col = "local", c("parc", "transc"), sep = '_')

#Join everything =========================
index = index %>% 
  mutate(local2 = local) %>% 
  mutate(data2 = data) %>% 
  unite(col = "id", c("local2", "data2"), sep = '_')

biomass = biomass %>% 
  mutate(local2 = local) %>% 
  mutate(data2 = data) %>% 
  unite(col = "id", c("local2", "data2"), sep = '_')

fuel = fuel %>% 
  mutate(local2 = local) %>% 
  mutate(data2 = data) %>% 
  unite(col = "id", c("local2", "data2"), sep = '_')


index = full_join(index, biomass, by="id")
index = full_join(index, fuel, by="id")

#index = index %>% 
# separate(col = "id", c("parcela", "data"), sep = '_')

index = index[,c(3,1,2,8,14)]
colnames(index) = c("Parcela", "NDII", "LWVI2", "Biomass", "Fuel")

index = na.omit(index)

ggpairs(index)















